module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule as Rule exposing (Rule)
import NoAlways
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoUnapprovedLicense
import NoUnoptimizedRecursion
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Simplify


config : List Rule
config =
    [ NoAlways.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Internal/Test.elm" ]
    , NoImportingEverything.rule
        [ "Internal.Test"
        ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
    , NoUnapprovedLicense.rule
        { allowed = [ "BSD-3-Clause", "MIT" ]
        , forbidden = [ "GPL-3.0-only", "GPL-3.0-or-later" ]
        }
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    , NoUnused.CustomTypeConstructorArgs.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Internal/Test.elm"
            , "src/Internal.elm"
            ]
    , NoUnused.CustomTypeConstructors.rule []
        |> Rule.ignoreErrorsForFiles
            [ "src/Internal/Markup.elm"
            ]
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    ]
    |> List.map
        ( Rule.ignoreErrorsForDirectories
            [ "tests/VerifyExamples"
            ]
        )
