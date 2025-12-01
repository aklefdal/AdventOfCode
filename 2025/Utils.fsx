open System
open System.Text.RegularExpressions

let toInts (line: string) =
    line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int

let trim (s: string) = s.Trim()

let splitLines (s: string) = s.Split([| '\n' |]) |> Array.map trim

let split (separator: string) (s: string) = s.Split(separator)

let join (separator: string) (lines: string seq) = String.Join(separator, lines)

let regexMatches (pattern: string) (s: string) = Regex.Matches(s, pattern)

let toChars (s: string) = s.ToCharArray()

let isAllNumeric (s: string) =
    s |> toChars |> Array.forall Char.IsDigit
