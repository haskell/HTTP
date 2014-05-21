module UnitTests ( unitTests ) where

import Network.HTTP.Base
import Network.URI

import Data.Maybe ( fromJust )

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

unitTests =
    [testGroup "Unit tests"
        [ testGroup "URI parsing"
            [ testCase "Parse IPv4 address" parseIPv4Address
            , testCase "Parse IPv6 address" parseIPv6Address
            ]
        ]
    ]
