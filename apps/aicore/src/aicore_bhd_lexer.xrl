Definitions.

WEEKDAY = [mon|tue|wed|thu|fri|sat|sun]+
DASH = -+
DIGIT = [0-9]
TIME_DELIMITER = :
WHITESPACE = [\s\t\n\r]

Rules.
{WEEKDAY}         : {token, {weekday, TokenLine, TokenChars}}.
{DASH}            : {token, {'-', TokenLine}}.
\[                : {token, {'[',  TokenLine}}.
\]                : {token, {']',  TokenLine}}.
{DIGIT}{DIGIT}    : {token, {time, TokenLine, TokenChars}}.
{TIME_DELIMITER}  : {token, {time_delimiter, TokenLine, TokenChars}}.
{WHITESPACE}      : skip_token.

Erlang code.

