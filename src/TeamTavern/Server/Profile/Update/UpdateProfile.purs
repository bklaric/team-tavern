module TeamTavern.Server.Profile.Update.UpdateProfile
    (UpdateProfileError, updateProfile) where

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
import TeamTavern.Server.Game.Domain.Handle (Handle)
import TeamTavern.Server.Infrastructure.Cookie (CookieInfo)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Profile.Domain.FieldValue (FieldValue(..), FieldValueType(..))
import TeamTavern.Server.Profile.Domain.Summary (Summary)
import TeamTavern.Server.Profile.Infrastructure.ReadProfile (Profile)
import TeamTavern.Server.Profile.Routes (Identifiers)
import Unsafe.Coerce (unsafeCoerce)

type UpdateProfileError errors = Variant
    ( databaseError :: Error
    , notAuthorized ::
        { cookieInfo :: CookieInfo
        , identifiers ::
            { handle :: Handle
            , nickname :: Nickname
            }
        }
    , unreadableProfileId ::
        { result :: Result
        , errors :: MultipleErrors
        }
    | errors )

updateProfileString :: Query
updateProfileString = Query """
    update profile
    set summary = $5
    from session, player, game
    where session.player_id = $1
    and session.token = $2
    and session.revoked = false
    and session.player_id = player.id
    and player.id = profile.player_id
    and game.id = profile.game_id
    and player.nickname = $3
    and game.handle = $4
    returning profile.id as "profileId";
    """

updateProfileParameters ::
    CookieInfo -> Identifiers -> Summary -> Array QueryParameter
updateProfileParameters { id, token } { nickname, handle } summary =
    id : token : nickname : handle :| summary

deleteFieldValuesString :: Query
deleteFieldValuesString = Query """
    delete from field_value
    where profile_id = $1;
    """
deleteFieldValuesParameters :: Int -> Array QueryParameter
deleteFieldValuesParameters profileId = profileId : []

insertFieldValuesString :: Query
insertFieldValuesString = Query """
    insert into field_value (profile_id, field_id, data)
    select *
    from unnest ($1::int[], $2::int[], $3::jsonb[]);
    """

insertFieldValuesParameters :: Int -> List FieldValue -> Array QueryParameter
insertFieldValuesParameters profileId fieldValues =
    fieldValues
    # foldr (\(FieldValue fieldId type') { profileIds, fieldIds, datas } ->
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
    # \{ profileIds, fieldIds, datas } ->
        profileIds : fieldIds :| datas

updateProfile
    :: forall errors
    .  Client
    -> CookieInfo
    -> Identifiers
    -> Profile
    -> Async (UpdateProfileError errors) Unit
updateProfile client cookieInfo identifiers { summary, fieldValues } = do
    result <- client
        # query updateProfileString
            (updateProfileParameters cookieInfo (unsafeCoerce identifiers) (unsafeCoerce summary))
        # label (SProxy :: SProxy "databaseError")
    { profileId } :: { profileId :: Int } <- rows result
        # head
        # Async.note
            (inj (SProxy :: SProxy "notAuthorized") { cookieInfo, identifiers: unsafeCoerce identifiers })
        >>= (read >>> labelMap (SProxy :: SProxy "unreadableProfileId")
            { result, errors: _ })
    client
        # execute deleteFieldValuesString
            (deleteFieldValuesParameters profileId)
        # label (SProxy :: SProxy "databaseError")
    client
        # execute insertFieldValuesString
            (insertFieldValuesParameters profileId (unsafeCoerce fieldValues))
        # label (SProxy :: SProxy "databaseError")
