module TeamTavern.Server.Profile.Infrastructure.AddProfile
    (AddProfileError, addProfile) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (head)
import Data.Array as Array
import Data.Bifunctor.Label (label, labelMap)
import Data.List (List, foldr)
import Data.Variant (SProxy(..), Variant, inj)
import Foreign (MultipleErrors)
import Postgres.Async.Query (execute, query)
import Postgres.Client (Client)
import Postgres.Error (Error)
import Postgres.Query (Query(..), QueryParameter, (:), (:|))
import Postgres.Result (Result, rows)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Profile.Domain.FieldValue (FieldValue(..), FieldValueType(..))
import TeamTavern.Server.Profile.Domain.Summary (Summary)
import TeamTavern.Server.Profile.Infrastructure.ReadProfile (ProfileModel)
import TeamTavern.Server.Profile.Routes (Identifiers)

type AddProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , identifiers :: Identifiers
        }
    , unreadableProfileId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

insertProfileString :: Query
insertProfileString = Query """
    insert into profile (player_id, game_id, summary)
    select player.id, game.id, $5
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and game.handle = $3
    and player.nickname = $4
    returning profile.id as "profileId";
    """

insertProfileParameters :: CookieInfo -> Identifiers -> Summary -> Array QueryParameter
insertProfileParameters { id, token } { handle, nickname } summary =
    id : token : handle : nickname :| summary

insertFieldValuesString :: Query
insertFieldValuesString = Query """
    insert into field_value (profile_id, field_id, data)
    select *
    from unnest ($1::int[], $2::int[], $3::jsonb[]);
    """

insertFieldValuesParameters :: Int -> List FieldValue -> Array QueryParameter
insertFieldValuesParameters profileId fieldValues =
    foldr (\(FieldValue fieldId type') { profileIds, fieldIds, datas } ->
            case type' of
            UrlValue url ->
                { profileIds: profileId : profileIds
                , fieldIds: fieldId : fieldIds
                , datas: { url } : datas
                }
            SingleValue optionId ->
                { profileIds: profileId : profileIds
                , fieldIds: fieldId : fieldIds
                , datas: { optionId } : datas
                }
            MultiValue optionIds ->
                { profileIds: profileId : profileIds
                , fieldIds: fieldId : fieldIds
                , datas: { optionIds: Array.fromFoldable optionIds } : datas
                }
            )
        { profileIds: [], fieldIds: [], datas: [] }
        fieldValues
    # \{ profileIds, fieldIds, datas } ->
        profileIds : fieldIds :| datas

addProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> Identifiers
    -> ProfileModel
    -> Async (AddProfileError errors) Unit
addProfile client cookieInfo identifiers { summary, fieldValues } = do
    result <- client
        # query insertProfileString (insertProfileParameters cookieInfo identifiers summary)
        # label (SProxy :: SProxy "databaseError")
    { profileId } :: { profileId :: Int } <- rows result
        # head
        # Async.note (inj (SProxy :: SProxy "notAuthorized") { cookieInfo, identifiers })
        >>= (read >>> labelMap (SProxy :: SProxy "unreadableProfileId") { result, errors: _ })
    client
        # execute insertFieldValuesString (insertFieldValuesParameters profileId fieldValues)
        # label (SProxy :: SProxy "databaseError")
