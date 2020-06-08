module UnitTests ( unitTests ) where

import Network.HTTP.Base
import Network.HTTP.Headers
import Network.URI

import Data.Maybe ( fromJust )
import Data.Either ( fromRight )

import Test.Framework ( testGroup )
import Test.Framework.Providers.HUnit
import Test.HUnit

parseIPv4Address :: Assertion
parseIPv4Address =
    assertEqual "127.0.0.1 address is recognised"
         (Just (URIAuthority {user = Nothing, password = Nothing, host = "127.0.0.1", port = Just 5313}))
         (parseURIAuthority (uriToAuthorityString (fromJust (parseURI "http://127.0.0.1:5313/foo"))))


parseIPv6Address :: Assertion
parseIPv6Address =
    assertEqual "::1 address"
         (Just (URIAuthority {user = Nothing, password = Nothing, host = "::1", port = Just 5313}))
         (parseURIAuthority (uriToAuthorityString (fromJust (parseURI "http://[::1]:5313/foo"))))

customHeaderNameComparison :: Assertion
customHeaderNameComparison =
    assertEqual "custom header name" (HdrCustom "foo") (HdrCustom "Foo")

customHeaderLookup :: Assertion
customHeaderLookup =
    let val = "header value"
        h = Header (HdrCustom "foo") val

    in assertEqual "custom header lookup" (Just val)
        (lookupHeader (HdrCustom "Foo") [h])

caseInsensitiveHeaderParse :: Assertion
caseInsensitiveHeaderParse =
    let expected = [ Header HdrContentType "blah"
                   , Header (HdrCustom "X-Unknown") "unused"
                   ]
        input = [ "content-type: blah"
                , "X-Unknown: unused"
                ]

        match actual =
            length actual == length expected &&
            and [ hdrName a == hdrName b && hdrValue a == hdrValue b
                | (a, b) <- zip expected actual
                ]

    in case parseHeaders input of
        Left _ -> assertFailure "Failed header parse"
        Right actual -> assert (match actual)

unitTests =
    [testGroup "Unit tests"
        [ testGroup "URI parsing"
            [ testCase "Parse IPv4 address" parseIPv4Address
            , testCase "Parse IPv6 address" parseIPv6Address
            ]
        ]
    , testGroup "Header tests"
        [ testCase "Custom header name case-insensitive match" customHeaderNameComparison
        , testCase "Custom header lookup" customHeaderLookup
        , testCase "Case-insensitive parsing" caseInsensitiveHeaderParse
        ]
    ]
