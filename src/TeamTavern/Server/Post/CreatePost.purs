module TeamTavern.Server.Post.CreatePost (createPost) where

import Prelude

import Async (Async, note)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Variant (Variant, inj)
import Effect.Class (liftEffect)
import Effect.Now (nowDate)
import Jarilo (BadRequestRow, InternalRow_, badRequest_, internal__, noContent_)
import JavaScript.Node.Errors.Class (code)
import JavaScript.Npm.Pg.Async (query)
import JavaScript.Npm.Pg.Client (Client)
import JavaScript.Npm.Pg.Error (constraint)
import JavaScript.Npm.Pg.Error.Codes (unique_violation)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import JavaScript.Npm.Pg.Result (rows)
import TeamTavern.Routes.Shared.Post (RequestContent)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedIn (ensureSignedIn)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.GenerateNonce (generateNonce)
import TeamTavern.Server.Infrastructure.GenerateNonce as Nonce
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines, transaction)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Post.Infrastructure.LoadCatalogue (loadCatalogue)
import TeamTavern.Server.Post.Infrastructure.ValidatePost (ValidPost, validatePost)
import TeamTavern.Server.Post.Infrastructure.WriteAccount (writeAccount)
import TeamTavern.Server.Post.Infrastructure.WriteAnswers (writeAnswers)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Yoga.JSON.Async (read)

type InsertError errors errors' = TerrorVar
    ( InternalRow_
    + BadRequestRow (Variant (exists :: {} | errors'))
    + errors )

insertQuery :: Query
insertQuery = Query """
    insert into post
        ( player_id, game_id, ilk, renewal_nonce, summary, microphone
        , online_from, online_to, contact_preference, name, regions, languages
        , website, discord_server, age_from, age_to
        , group_size, group_wanted_from, group_wanted_to
        )
    values
        ( $1, $2, $3, $4, $5::text[], $6
        , $7::time, $8::time, $9, $10, $11::text[], $12::text[]
        , $13, $14, $15::integer, $16::integer
        , $17::integer, $18::integer, $19::integer
        )
    returning id
    """

insertPost :: ∀ errors errors'.
    Client -> { playerId :: Int, gameId :: Int, type_ :: String, nonce :: String } -> ValidPost
    -> Async (InsertError errors errors') Int
insertPost client { playerId, gameId, type_, nonce } { post, summary } = do
    result <- client # query insertQuery
        ( playerId : gameId : type_ : nonce : summary : post.microphone
        : toNullable (post.online <#> _.from) : toNullable (post.online <#> _.to)
        : post.contactPreference : toNullable post.name : post.regions : post.languages
        : toNullable post.website : toNullable post.discordServer
        : toNullable post.ageFrom : toNullable post.ageTo
        : toNullable post.groupSize : toNullable post.groupWantedFrom :| toNullable post.groupWantedTo
        )
        # lmap \error -> case code error == unique_violation, constraint error of
            true, Just "post_player_id_game_id_ilk_key" -> Terror
                (badRequest_ $ inj (Proxy :: _ "exists") {})
                [ "Player already has a " <> type_ <> " post in the game.", print error ]
            _, _ -> Terror internal__ $ databaseErrorLines error
    row <- result # rows # head # note (Terror internal__ [ "Expected post id in query result, got no rows." ])
    row # (read :: _ -> _ _ { id :: Int }) <#> _.id
        # lmap \error -> Terror internal__ [ "Error reading post id: " <> show error ]

-- | Publishes a post, writing the account facts and contacts it gave to the
-- | account in the same transaction (brief 6, step 3).
createPost :: ∀ left. Pool -> String -> String -> Cookies -> RequestContent -> Async left _
createPost pool handle type_ cookies content =
    sendResponse "Error creating post" do
    { id } <- ensureSignedIn pool cookies
    { gameId, game, countries } <- loadCatalogue pool handle type_
    today <- liftEffect nowDate
    valid <- validatePost game countries type_ today content
    nonce <- generateNonce
    let playerId = unwrap id
    pool # transaction \client -> do
        writeAccount client playerId valid.account
        postId <- insertPost client { playerId, gameId, type_, nonce: Nonce.toString nonce } valid
        writeAnswers client gameId postId valid.post
    pure noContent_
