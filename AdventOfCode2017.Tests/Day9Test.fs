module Day9Test

open Swensen.Unquote
open Xunit
open Day9

[<Theory>]
[<InlineData("{{<!>},{<!>},{<!>},{<a>}}", "{{<},{<},{<},{<a>}}")>]
[<InlineData("<!!!>>", "<>")>]
let ``Ignore char following exclamation mark``(str, expected) =
    let escaped = deleteCancelleds str
    test <@ escaped = expected @>

[<Theory>]
[<InlineData("{{<!>},{<!>},{<!>},{<a>}}", "{{}}")>]
[<InlineData("<!!!>>", "")>]
[<InlineData("<{o\"i!a,<{i<a>", "")>]
[<InlineData("{<a>,<a>,<a>,<a>}", "{,,,}")>]
let ``Ignore garbage``(str, expected) =
    let escaped = deleteGarbage str
    test <@ escaped = expected @>

[<Theory>]
[<InlineData("{}", 1)>]
[<InlineData("{{{}}}", 6)>]
[<InlineData("{{{},{},{{}}}}", 16)>]
[<InlineData("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)>]
[<InlineData("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)>]
let ``Count groups``(str, expected) =
    let groups = countGroups str
    test <@ groups = expected @>

[<Theory>]
[<InlineData("<>", 0)>]
[<InlineData("<random characters>", 17)>]
[<InlineData("<<<<>", 3)>]
[<InlineData("<{!>}>", 2)>]
[<InlineData("<!!>", 0)>]
[<InlineData("<!!!>>", 0)>]
[<InlineData("<{o\"i!a,<{i<a>", 10)>]
let ``Count garbage chars``(str, expected) =
    let groups = countGarbage str
    test <@ groups = expected @>