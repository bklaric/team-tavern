module TeamTavern.Server.Post.Infrastructure.Answers (flagsJson, optionsJson, rangesJson) where

-- | A post's answers to its game's fields, keyed by the fields' own keys, as
-- | `Feed.sql` reads a description: SQL expressions over a `post` in scope.

-- | Each field's chosen option keys, in the options' order.
optionsJson :: String
optionsJson = """
    coalesce((
        select jsonb_object_agg(field.key, options)
        from (
            select option.field_id, jsonb_agg(option.key order by option.ordinal) as options
            from post_field_option answer
            join field_option option on option.id = answer.field_option_id
            where answer.post_id = post.id
            group by option.field_id
        ) answers
        join field on field.id = answers.field_id
    ), '{}')
    """

-- | Each ordered field's ends, either of which may be open.
rangesJson :: String
rangesJson = """
    coalesce((
        select jsonb_object_agg(field.key, jsonb_build_object('from', range_from.key, 'to', range_to.key))
        from post_field_range range
        join field on field.id = range.field_id
        left join field_option range_from on range_from.id = range.from_option_id
        left join field_option range_to on range_to.id = range.to_option_id
        where range.post_id = post.id
    ), '{}')
    """

-- | The boolean fields answered yes, in the game's order.
flagsJson :: String
flagsJson = """
    to_jsonb(array(
        select field.key
        from post_field_flag flag
        join field on field.id = flag.field_id
        where flag.post_id = post.id
        order by field.ordinal
    ))
    """
