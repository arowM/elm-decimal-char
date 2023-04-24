module DecimalChar exposing
    ( isNumeral
    , isBasicLatin
    , toBasicLatin
    , normalize
    )

{-| Provide functions to handle Unicode decimal characters, which are listed in [DerivedNumericType-15.0.0.txt](https://www.unicode.org/Public/15.0.0/ucd/extracted/DerivedNumericType.txt) as `Numeric_Type=Decimal`.

You can use this library to normalize user inputs, which will free users from the hassle of entering numbers in the "correct" format.

@docs isNumeral
@docs isBasicLatin
@docs toBasicLatin
@docs normalize

-}


{-| Check if a character is the Basic Latin digit (i.e., between 0x0030 and 0x0039 in code points).

    isBasicLatin '3'
    --> True

    isBasicLatin '３'
    --> False

-}
isBasicLatin : Char -> Bool
isBasicLatin c =
    let
        codepoint =
            Char.toCode c
    in
    0x30 <= codepoint && codepoint <= 0x39


{-| Check if a character is decimal (i.e., \`Numeric\_Type=Decimal).

    isNumeral '3'
    --> True

    isNumeral '３'
    --> True

    isNumeral '🐐'
    --> False

    -- Roman digit is numeric, but not decimal.
    isNumeral 'Ⅵ'
    --> False

-}
isNumeral : Char -> Bool
isNumeral c =
    toBasicLatin c /= Nothing


{-| Convert a character to the corresponding Basic Latin digit character.

If the character is not decimal, it returns `Nothing`.

    toBasicLatin '３'
    --> Just '3'

    toBasicLatin '٩'
    --> Just '9'

    toBasicLatin '🐐'
    --> Nothing

-}
toBasicLatin : Char -> Maybe Char
toBasicLatin c =
    let
        codepoint =
            Char.toCode c
    in
    if 0x30 <= codepoint && codepoint <= 0x39 then
        -- DIGIT ZERO..DIGIT NINE
        Just c

    else if 0x0660 <= codepoint && codepoint <= 0x0669 then
        -- ARABIC-INDIC DIGIT ZERO..ARABIC-INDIC DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0660)

    else if 0x06F0 <= codepoint && codepoint <= 0x06F9 then
        -- EXTENDED ARABIC-INDIC DIGIT ZERO..EXTENDED ARABIC-INDIC DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x06F0)

    else if 0x07C0 <= codepoint && codepoint <= 0x07C9 then
        -- NKO DIGIT ZERO..NKO DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x07C0)

    else if 0x0966 <= codepoint && codepoint <= 0x096F then
        -- DEVANAGARI DIGIT ZERO..DEVANAGARI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0966)

    else if 0x09E6 <= codepoint && codepoint <= 0x09EF then
        -- BENGALI DIGIT ZERO..BENGALI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x09E6)

    else if 0x0A66 <= codepoint && codepoint <= 0x0A6F then
        -- GURMUKHI DIGIT ZERO..GURMUKHI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0A66)

    else if 0x0AE6 <= codepoint && codepoint <= 0x0AEF then
        -- GUJARATI DIGIT ZERO..GUJARATI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0AE6)

    else if 0x0B66 <= codepoint && codepoint <= 0x0B6F then
        -- ORIYA DIGIT ZERO..ORIYA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0B66)

    else if 0x0BE6 <= codepoint && codepoint <= 0x0BEF then
        -- TAMIL DIGIT ZERO..TAMIL DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0BE6)

    else if 0x0C66 <= codepoint && codepoint <= 0x0C6F then
        -- TELUGU DIGIT ZERO..TELUGU DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0C66)

    else if 0x0CE6 <= codepoint && codepoint <= 0x0CEF then
        -- KANNADA DIGIT ZERO..KANNADA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0CE6)

    else if 0x0D66 <= codepoint && codepoint <= 0x0D6F then
        -- MALAYALAM DIGIT ZERO..MALAYALAM DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0D66)

    else if 0x0DE6 <= codepoint && codepoint <= 0x0DEF then
        -- SINHALA LITH DIGIT ZERO..SINHALA LITH DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0DE6)

    else if 0x0E50 <= codepoint && codepoint <= 0x0E59 then
        -- THAI DIGIT ZERO..THAI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0E50)

    else if 0x0ED0 <= codepoint && codepoint <= 0x0ED9 then
        -- LAO DIGIT ZERO..LAO DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0ED0)

    else if 0x0F20 <= codepoint && codepoint <= 0x0F29 then
        -- TIBETAN DIGIT ZERO..TIBETAN DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0F20)

    else if 0x1040 <= codepoint && codepoint <= 0x1049 then
        -- MYANMAR DIGIT ZERO..MYANMAR DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1040)

    else if 0x1090 <= codepoint && codepoint <= 0x1099 then
        -- MYANMAR SHAN DIGIT ZERO..MYANMAR SHAN DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1090)

    else if 0x17E0 <= codepoint && codepoint <= 0x17E9 then
        -- KHMER DIGIT ZERO..KHMER DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x17E0)

    else if 0x1810 <= codepoint && codepoint <= 0x1819 then
        -- MONGOLIAN DIGIT ZERO..MONGOLIAN DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1810)

    else if 0x1946 <= codepoint && codepoint <= 0x194F then
        -- LIMBU DIGIT ZERO..LIMBU DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1946)

    else if 0x19D0 <= codepoint && codepoint <= 0x19D9 then
        -- NEW TAI LUE DIGIT ZERO..NEW TAI LUE DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x19D0)

    else if 0x1A80 <= codepoint && codepoint <= 0x1A89 then
        -- TAI THAM HORA DIGIT ZERO..TAI THAM HORA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1A80)

    else if 0x1A90 <= codepoint && codepoint <= 0x1A99 then
        -- TAI THAM THAM DIGIT ZERO..TAI THAM THAM DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1A90)

    else if 0x1B50 <= codepoint && codepoint <= 0x1B59 then
        -- BALINESE DIGIT ZERO..BALINESE DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1B50)

    else if 0x1BB0 <= codepoint && codepoint <= 0x1BB9 then
        -- SUNDANESE DIGIT ZERO..SUNDANESE DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1BB0)

    else if 0x1C40 <= codepoint && codepoint <= 0x1C49 then
        -- LEPCHA DIGIT ZERO..LEPCHA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1C40)

    else if 0x1C50 <= codepoint && codepoint <= 0x1C59 then
        -- OL CHIKI DIGIT ZERO..OL CHIKI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x1C50)

    else if 0xA620 <= codepoint && codepoint <= 0xA629 then
        -- VAI DIGIT ZERO..VAI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0xA620)

    else if 0xA8D0 <= codepoint && codepoint <= 0xA8D9 then
        -- SAURASHTRA DIGIT ZERO..SAURASHTRA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0xA8D0)

    else if 0xA900 <= codepoint && codepoint <= 0xA909 then
        -- KAYAH LI DIGIT ZERO..KAYAH LI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0xA900)

    else if 0xA9D0 <= codepoint && codepoint <= 0xA9D9 then
        -- JAVANESE DIGIT ZERO..JAVANESE DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0xA9D0)

    else if 0xA9F0 <= codepoint && codepoint <= 0xA9F9 then
        -- MYANMAR TAI LAING DIGIT ZERO..MYANMAR TAI LAING DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0xA9F0)

    else if 0xAA50 <= codepoint && codepoint <= 0xAA59 then
        -- CHAM DIGIT ZERO..CHAM DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0xAA50)

    else if 0xABF0 <= codepoint && codepoint <= 0xABF9 then
        -- MEETEI MAYEK DIGIT ZERO..MEETEI MAYEK DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0xABF0)

    else if 0xFF10 <= codepoint && codepoint <= 0xFF19 then
        -- FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0xFF10)

    else if 0x000104A0 <= codepoint && codepoint <= 0x000104A9 then
        -- OSMANYA DIGIT ZERO..OSMANYA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x000104A0)

    else if 0x00010D30 <= codepoint && codepoint <= 0x00010D39 then
        -- HANIFI ROHINGYA DIGIT ZERO..HANIFI ROHINGYA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00010D30)

    else if 0x00011066 <= codepoint && codepoint <= 0x0001106F then
        -- BRAHMI DIGIT ZERO..BRAHMI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011066)

    else if 0x000110F0 <= codepoint && codepoint <= 0x000110F9 then
        -- SORA SOMPENG DIGIT ZERO..SORA SOMPENG DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x000110F0)

    else if 0x00011136 <= codepoint && codepoint <= 0x0001113F then
        -- CHAKMA DIGIT ZERO..CHAKMA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011136)

    else if 0x000111D0 <= codepoint && codepoint <= 0x000111D9 then
        -- SHARADA DIGIT ZERO..SHARADA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x000111D0)

    else if 0x000112F0 <= codepoint && codepoint <= 0x000112F9 then
        -- KHUDAWADI DIGIT ZERO..KHUDAWADI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x000112F0)

    else if 0x00011450 <= codepoint && codepoint <= 0x00011459 then
        -- NEWA DIGIT ZERO..NEWA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011450)

    else if 0x000114D0 <= codepoint && codepoint <= 0x000114D9 then
        -- TIRHUTA DIGIT ZERO..TIRHUTA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x000114D0)

    else if 0x00011650 <= codepoint && codepoint <= 0x00011659 then
        -- MODI DIGIT ZERO..MODI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011650)

    else if 0x000116C0 <= codepoint && codepoint <= 0x000116C9 then
        -- TAKRI DIGIT ZERO..TAKRI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x000116C0)

    else if 0x00011730 <= codepoint && codepoint <= 0x00011739 then
        -- AHOM DIGIT ZERO..AHOM DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011730)

    else if 0x000118E0 <= codepoint && codepoint <= 0x000118E9 then
        -- WARANG CITI DIGIT ZERO..WARANG CITI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x000118E0)

    else if 0x00011950 <= codepoint && codepoint <= 0x00011959 then
        -- DIVES AKURU DIGIT ZERO..DIVES AKURU DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011950)

    else if 0x00011C50 <= codepoint && codepoint <= 0x00011C59 then
        -- BHAIKSUKI DIGIT ZERO..BHAIKSUKI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011C50)

    else if 0x00011D50 <= codepoint && codepoint <= 0x00011D59 then
        -- MASARAM GONDI DIGIT ZERO..MASARAM GONDI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011D50)

    else if 0x00011DA0 <= codepoint && codepoint <= 0x00011DA9 then
        -- GUNJALA GONDI DIGIT ZERO..GUNJALA GONDI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011DA0)

    else if 0x00011F50 <= codepoint && codepoint <= 0x00011F59 then
        -- KAWI DIGIT ZERO..KAWI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00011F50)

    else if 0x00016A60 <= codepoint && codepoint <= 0x00016A69 then
        -- MRO DIGIT ZERO..MRO DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00016A60)

    else if 0x00016AC0 <= codepoint && codepoint <= 0x00016AC9 then
        -- TANGSA DIGIT ZERO..TANGSA DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00016AC0)

    else if 0x00016B50 <= codepoint && codepoint <= 0x00016B59 then
        -- PAHAWH HMONG DIGIT ZERO..PAHAWH HMONG DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x00016B50)

    else if 0x0001D7CE <= codepoint && codepoint <= 0x0001D7FF then
        -- MATHEMATICAL BOLD DIGIT ZERO..MATHEMATICAL MONOSPACE DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0001D7CE)

    else if 0x0001E140 <= codepoint && codepoint <= 0x0001E149 then
        -- NYIAKENG PUACHUE HMONG DIGIT ZERO..NYIAKENG PUACHUE HMONG DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0001E140)

    else if 0x0001E2F0 <= codepoint && codepoint <= 0x0001E2F9 then
        -- WANCHO DIGIT ZERO..WANCHO DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0001E2F0)

    else if 0x0001E4F0 <= codepoint && codepoint <= 0x0001E4F9 then
        -- NAG MUNDARI DIGIT ZERO..NAG MUNDARI DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0001E4F0)

    else if 0x0001E950 <= codepoint && codepoint <= 0x0001E959 then
        -- ADLAM DIGIT ZERO..ADLAM DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0001E950)

    else if 0x0001FBF0 <= codepoint && codepoint <= 0x0001FBF9 then
        -- SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
        Just <| Char.fromCode (0x30 + codepoint - 0x0001FBF0)

    else
        Nothing


{-| Extract only the decimal characters contained in a given string and convert them to the corresponding Basic Latin characters.

    normalize "０１２３🐐４５٩🌱"
    --> "0123459"

-}
normalize : String -> String
normalize =
    String.foldr
        (\c acc ->
            case toBasicLatin c of
                Nothing ->
                    acc

                Just digit ->
                    String.cons digit acc
        )
        ""
