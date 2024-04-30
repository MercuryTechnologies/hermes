{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
module Network.HTTP.Headers.HeaderFieldName
  ( HeaderFieldName
  , toText
  , toCIByteString
  , cachedHeaderFieldName
  , headerFieldName
  , unsafeCachedHeaderFromBytestring
  , unsafeUnknownHeaderFromBytestring
  , headerNameToShortByteString
  , headerNameFromShortByteString
-- * IANA-registered HTTP Header Field Names
  -- ** Permanent Message Header Field Names
  , hAIM
  , hAccept
  , hAcceptAdditions
  , hAcceptCH
  , hAcceptDatetime
  , hAcceptEncoding
  , hAcceptFeatures
  , hAcceptLanguage
  , hAcceptPatch
  , hAcceptPost
  , hAcceptRanges
  , hAcceptSignature
  , hAccessControlAllowCredentials
  , hAccessControlAllowHeaders
  , hAccessControlAllowMethods
  , hAccessControlAllowOrigin
  , hAccessControlExposeHeaders
  , hAccessControlMaxAge
  , hAccessControlRequestHeaders
  , hAccessControlRequestMethod
  , hAge
  , hAllow
  , hALPN
  , hAltSvc
  , hAltUsed
  , hAlternates
  , hApplyToRedirectRef
  , hAuthenticationControl
  , hAuthenticationInfo
  , hAuthorization
  , hCacheControl
  , hCacheStatus
  , hCalManagedID
  , hCalDAVTimezones
  , hCapsuleProtocol
  , hCDNCacheControl
  , hCDNLoop
  , hCertNotAfter
  , hCertNotBefore
  , hClearSiteData
  , hClientCert
  , hClientCertChain
  , hClose
  , hConnection
  , hContentDisposition
  , hContentEncoding
  , hContentLanguage
  , hContentLength
  , hContentLocation
  , hContentRange
  , hContentSecurityPolicy
  , hContentSecurityPolicyReportOnly
  , hContentDigest
  , hContentType
  , hCookie
  , hCrossOriginEmbedderPolicy
  , hCrossOriginEmbedderPolicyReportOnly
  , hCrossOriginOpenerPolicy
  , hCrossOriginOpenerPolicyReportOnly
  , hCrossOriginResourcePolicy
  , hDASL
  , hDate
  , hDAV
  , hDeltaBase
  , hDepth
  , hDestination
  , hDPoP
  , hDPoPNonce
  , hEarlyData
  , hEDIINTFeatures
  , hETag
  , hExpect
  , hExpires
  , hForwarded
  , hFrom
  , hHobareg
  , hHost
  , hIf
  , hIfMatch
  , hIfModifiedSince
  , hIfNoneMatch
  , hIfRange
  , hIfScheduleTagMatch
  , hIfUnmodifiedSince
  , hIM
  , hIncludeReferredTokenBindingID
  , hIsolation
  , hKeepAlive
  , hLabel
  , hLastEventID
  , hLastModified
  , hLink
  , hLocation
  , hLockToken
  , hMaxForwards
  , hMementoDatetime
  , hMeter
  , hMIMEVersion
  , hNegotiate
  , hNEL
  , hODataEntityId
  , hODataIsolation
  , hODataMaxVersion
  , hODataVersion
  , hOptionalWWWAuthenticate
  , hOrderingType
  , hOrigin
  , hOriginAgentCluster
  , hOSCORE
  , hOSLCCoreVersion
  , hOverwrite
  , hPermissionsPolicy
  , hPingFrom
  , hPingTo
  , hPosition
  , hPrefer
  , hPreferenceApplied
  , hPriority
  , hProxyAuthenticate
  , hProxyAuthenticationInfo
  , hProxyAuthorization
  , hProxyStatus
  , hPublicKeyPins
  , hPublicKeyPinsReportOnly
  , hRange
  , hRedirectRef
  , hReferer
  , hRefresh
  , hReplayNonce
  , hReportingEndpoints
  , hReprDigest
  , hRetryAfter
  , hScheduleReply
  , hScheduleTag
  , hSecGPC
  , hSecPurpose
  , hSecTokenBinding
  , hSecWebSocketAccept
  , hSecWebSocketExtensions
  , hSecWebSocketKey
  , hSecWebSocketProtocol
  , hSecWebSocketVersion
  , hServer
  , hServerTiming
  , hSetCookie
  , hSignature
  , hSignatureInput
  , hSlug
  , hSoapAction
  , hStatusURI
  , hStrictTransportSecurity
  , hSunset
  , hSurrogateCapability
  , hSurrogateControl
  , hTCN
  , hTE
  , hTimeout
  , hTopic
  , hTraceparent
  , hTracestate
  , hTrailer
  , hTransferEncoding
  , hTTL
  , hUpgrade
  , hUrgency
  , hUserAgent
  , hVariantVary
  , hVary
  , hVia
  , hWantContentDigest
  , hWantReprDigest
  , hWWWAuthenticate
  , hXContentTypeOptions
  , hXFrameOptions
  , hWildcardHeader
  -- ** Provisional Message Header Field Names
  , hAMPCacheTransform
  , hConfigurationContext
  , hRepeatabilityClientID
  , hRepeatabilityFirstSent
  , hRepeatabilityRequestID
  , hRepeatabilityResult
  , hReportingEndpoints
  , hSurrogateCapability
  , hSurrogateControl
  , hTimingAllowOrigin
  -- ** Deprecated Message Header Field Names
  , hAcceptCharset
  , hContentID
  , hDifferentialID
  , hExpectCT
  , hPragma
  , hProtocolInfo
  , hProtocolQuery
  -- ** Obsoleted Message Header Field Names
  , hContentBase
  , hContentMD5
  , hContentScriptType
  , hContentStyleType
  , hContentVersion
  , hCookie2
  , hDefaultStyle
  , hDigest
  , hExt
  , hGetProfile
  , hHTTP2Settings
  , hMan
  , hMethodCheck
  , hMethodCheckExpires
  , hOpt
  , hP3P
  , hPEP
  , hPEPInfo
  , hPICSLabel
  , hProfileObject
  , hProtocol
  , hProtocolRequest
  , hProxyFeatures
  , hProxyInstruction
  , hPublicHeader
  , hRefererRoot
  , hSafe
  , hSetCookie2
  , hSetProfile
  , hURI
  , hWantDigest
  , hWarning
  ) where

import Control.Exception
import Data.Array.Byte.Hash (SipHash(..), SipKey(..), sipHash, unstableHashKey)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive.Unsafe (unsafeMk)
import Data.Char (toLower)
import Data.Hashable (Hashable(..))
import Data.Interned
import Data.Interned.Internal (recover)
import Data.Interned.Text
import Data.String (IsString(..))
import Data.Text.Internal (Text(..))
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding as TE
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | The type of a header field name.
data HeaderFieldName where
  HeaderFieldName :: {-# UNPACK #-} !SipHash -> {-# UNPACK #-} !Text -> HeaderFieldName

data InvalidHeaderFieldName = InvalidHeaderFieldName Text
  deriving (Show)

instance Exception InvalidHeaderFieldName

instance Interned HeaderFieldName where
  data Description HeaderFieldName = HeaderFieldNameDescription !SipHash
  type Uninterned HeaderFieldName = Text
  describe = HeaderFieldNameDescription . sipHash 1 3 unstableHashKey
  identify fid_ txt = HeaderFieldName (SipHash $ fromIntegral fid_) txt
  cache = hfsCache

deriving instance Eq (Description HeaderFieldName)
deriving instance Ord (Description HeaderFieldName)
instance Hashable (Description HeaderFieldName) where
  hash (HeaderFieldNameDescription (SipHash h)) = (fromIntegral h) :: Int
  hashWithSalt salt (HeaderFieldNameDescription (SipHash h)) = hashWithSalt salt ((fromIntegral h) :: Int)

instance IsString HeaderFieldName where
  fromString = cachedHeaderFieldName . fromString

hfsCache :: Cache HeaderFieldName
hfsCache = mkCache
{-# NOINLINE hfsCache #-}

toText :: HeaderFieldName -> Text
toText (HeaderFieldName _ name) = name

toCIByteString :: HeaderFieldName -> CI ByteString
toCIByteString = unsafeMk . TE.encodeUtf8 . toText

-- | Construct a 'HeaderFieldName' from a text input.
--
-- This function will intern the text for fast comparisons,
-- which makes it efficient for lookups on well-known header field names
-- and other frequently used headers.
cachedHeaderFieldName :: Text -> HeaderFieldName
cachedHeaderFieldName txt
  | T.isAscii txt = intern $ T.toLower txt
  | otherwise = throw $ InvalidHeaderFieldName txt
{-# INLINE cachedHeaderFieldName #-}

-- | Construct a 'HeaderFieldName' from a text input.
--
-- If the input is cached, this function will return the cached value.
-- If the input is not cached, will return a new 'HeaderFieldName' that is not cached.
--
-- This function is therefore suitable for arbitrary user input while reducing the
-- number of allocations and comparisons for well-known header field names.
headerFieldName :: Text -> HeaderFieldName
headerFieldName txt 
  | T.isAscii txt = unsafeDupablePerformIO $ do
      let caseFolded = T.toLower txt
          hashed = sipHash 1 3 unstableHashKey caseFolded
      mKnown <- recover $ HeaderFieldNameDescription hashed
      case mKnown of
        Just known -> pure known
        Nothing -> pure $! HeaderFieldName hashed caseFolded
  | otherwise = throw $ InvalidHeaderFieldName txt
{-# INLINE headerFieldName #-}

-- | Construct a 'HeaderFieldName' from a bytestring input that is a well-formed, case-folded header.
--
-- This function will interns the text, so it is not suitable for arbitrary user input.
--
-- __Warning__: This function is unsafe because it does not check if the input is valid ASCII,
-- a valid header field name according to the HTTP spec, and it does not fold the input to 
-- lowercase.
unsafeCachedHeaderFromBytestring :: ByteString -> HeaderFieldName
unsafeCachedHeaderFromBytestring bs =
  let !(SBS.SBS arr) = SBS.toShort bs
  in intern $ Text (A.ByteArray arr) 0 (BS.length bs)
{-# INLINE unsafeCachedHeaderFromBytestring #-}

-- | Construct a 'HeaderFieldName' from a bytestring input that is a well-formed, case-folded header.
--
-- This function will not intern the text, so it is suitable for arbitrary user input.
--
-- __Warning__: This function is unsafe because it does not check if the input is valid ASCII,
-- a valid header field name according to the HTTP spec, and it does not fold the input to 
-- lowercase.
unsafeUnknownHeaderFromBytestring :: ByteString -> HeaderFieldName
unsafeUnknownHeaderFromBytestring bs = unsafeDupablePerformIO $ do
  let !sbs@(SBS.SBS arr) = SBS.toShort bs
      hashed = sipHash 1 3 unstableHashKey sbs
  mKnown <- recover $ HeaderFieldNameDescription hashed
  case mKnown of
    Just known -> pure known
    Nothing -> pure $! HeaderFieldName hashed $ Text (A.ByteArray arr) 0 (BS.length bs)
{-# INLINE unsafeUnknownHeaderFromBytestring #-}

-- | Convert a 'HeaderFieldName' to a 'ShortByteString'.
headerNameToShortByteString :: HeaderFieldName -> SBS.ShortByteString
headerNameToShortByteString (HeaderFieldName _ (Text (A.ByteArray name) _ _)) = SBS.SBS name 

headerNameFromShortByteString :: SBS.ShortByteString -> HeaderFieldName
headerNameFromShortByteString !sbs@(SBS.SBS arr) = headerFieldName $ Text (A.ByteArray arr) 0 (SBS.length sbs)

instance Eq HeaderFieldName where
  HeaderFieldName _ a == HeaderFieldName _ b = a == b

instance Ord HeaderFieldName where
  HeaderFieldName _ a `compare` HeaderFieldName _ b = a `compare` b

instance Hashable HeaderFieldName where
  hash (HeaderFieldName (SipHash h) _) = fromIntegral h
  hashWithSalt salt (HeaderFieldName (SipHash h) _) = hashWithSalt salt (fromIntegral h :: Int)

instance Show HeaderFieldName where
  show (HeaderFieldName _ name) = show name

-- | 'A-IM' HTTP Header
-- Permanent: [RFC 3229: Delta encoding in HTTP](https://datatracker.ietf.org/doc/html/rfc3229)
hAIM :: HeaderFieldName
hAIM = "A-IM"

-- | 'Accept' HTTP Header
-- Permanent: [RFC9110, Section 12.5.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.1)
hAccept :: HeaderFieldName
hAccept = "Accept"

-- | 'Accept-Additions' HTTP Header
-- Permanent: [RFC 2324: Hyper Text Coffee Pot Control Protocol (HTCPCP/1.0)](https://datatracker.ietf.org/doc/html/rfc2324)
hAcceptAdditions :: HeaderFieldName
hAcceptAdditions = "Accept-Additions"

-- | 'Accept-CH' HTTP Header
-- Permanent: [RFC 8942, Section 3.1: HTTP Client Hints](https://datatracker.ietf.org/doc/html/rfc8942#section-3.1)
hAcceptCH :: HeaderFieldName
hAcceptCH = "Accept-CH"

-- | 'Accept-Charset' HTTP Header
-- Deprecated: [RFC9110, Section 12.5.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.2)
hAcceptCharset :: HeaderFieldName
hAcceptCharset = "Accept-Charset"

-- | 'Accept-Datetime' HTTP Header
-- Permanent: [RFC 7089: HTTP Framework for Time-Based Access to Resource States -- Memento](https://datatracker.ietf.org/doc/html/rfc7089)
hAcceptDatetime :: HeaderFieldName
hAcceptDatetime = "Accept-Datetime"

-- | 'Accept-Encoding' HTTP Header
-- Permanent: [RFC9110, Section 12.5.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.3)
hAcceptEncoding :: HeaderFieldName
hAcceptEncoding = "Accept-Encoding"

-- | 'Accept-Features' HTTP Header
-- Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)
hAcceptFeatures :: HeaderFieldName
hAcceptFeatures = "Accept-Features"

-- | 'Accept-Language' HTTP Header
-- Permanent: [RFC9110, Section 12.5.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.4)
hAcceptLanguage :: HeaderFieldName
hAcceptLanguage = "Accept-Language"

-- | 'Accept-Patch' HTTP Header
-- Permanent: [RFC 5789: PATCH Method for HTTP](https://datatracker.ietf.org/doc/html/rfc5789)
hAcceptPatch :: HeaderFieldName
hAcceptPatch = "Accept-Patch"

-- | 'Accept-Post' HTTP Header
-- Permanent: [Linked Data Platform 1.0](https://www.w3.org/TR/ldp/)
hAcceptPost :: HeaderFieldName
hAcceptPost = "Accept-Post"

-- | 'Accept-Ranges' HTTP Header
-- Permanent: [RFC9110, Section 14.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-14.3)
hAcceptRanges :: HeaderFieldName
hAcceptRanges = "Accept-Ranges"

-- | 'Accept-Signature' HTTP Header
-- Permanent: [RFC 9421, Section 5.1: HTTP Message Signatures](https://datatracker.ietf.org/doc/html/rfc9421#section-5.1)
hAcceptSignature :: HeaderFieldName
hAcceptSignature = "Accept-Signature"

-- | 'Access-Control-Allow-Credentials' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hAccessControlAllowCredentials :: HeaderFieldName
hAccessControlAllowCredentials = "Access-Control-Allow-Credentials"

-- | 'Access-Control-Allow-Headers' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hAccessControlAllowHeaders :: HeaderFieldName
hAccessControlAllowHeaders = "Access-Control-Allow-Headers"

-- | 'Access-Control-Allow-Methods' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hAccessControlAllowMethods :: HeaderFieldName
hAccessControlAllowMethods = "Access-Control-Allow-Methods"

-- | 'Access-Control-Allow-Origin' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hAccessControlAllowOrigin :: HeaderFieldName
hAccessControlAllowOrigin = "Access-Control-Allow-Origin"

-- | 'Access-Control-Expose-Headers' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hAccessControlExposeHeaders :: HeaderFieldName
hAccessControlExposeHeaders = "Access-Control-Expose-Headers"

-- | 'Access-Control-Max-Age' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hAccessControlMaxAge :: HeaderFieldName
hAccessControlMaxAge = "Access-Control-Max-Age"

-- | 'Access-Control-Request-Headers' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hAccessControlRequestHeaders :: HeaderFieldName
hAccessControlRequestHeaders = "Access-Control-Request-Headers"

-- | 'Access-Control-Request-Method' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hAccessControlRequestMethod :: HeaderFieldName
hAccessControlRequestMethod = "Access-Control-Request-Method"

-- | 'Age' HTTP Header
-- Permanent: [RFC9111, Section 5.1: HTTP Caching](https://datatracker.ietf.org/doc/html/rfc9111#section-5.1)
hAge :: HeaderFieldName
hAge = "Age"

-- | 'Allow' HTTP Header
-- Permanent: [RFC9110, Section 10.2.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.1)
hAllow :: HeaderFieldName
hAllow = "Allow"

-- | 'ALPN' HTTP Header
-- Permanent: [RFC 7639, Section 2: The ALPN HTTP Header Field](https://datatracker.ietf.org/doc/html/rfc7639#section-2)
hALPN :: HeaderFieldName
hALPN = "ALPN"

-- | 'Alt-Svc' HTTP Header
-- Permanent: [RFC 7838: HTTP Alternative Services](https://datatracker.ietf.org/doc/html/rfc7838)
hAltSvc :: HeaderFieldName
hAltSvc = "Alt-Svc"

-- | 'Alt-Used' HTTP Header
-- Permanent: [RFC 7838: HTTP Alternative Services](https://datatracker.ietf.org/doc/html/rfc7838)
hAltUsed :: HeaderFieldName
hAltUsed = "Alt-Used"

-- | 'Alternates' HTTP Header
-- Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)
hAlternates :: HeaderFieldName
hAlternates = "Alternates"

-- | 'AMP-Cache-Transform' HTTP Header
-- Provisional: [AMP-Cache-Transform HTTP request header](https://github.com/ampproject/amphtml/blob/main/spec/amp-cache-transform.md)
hAMPCacheTransform :: HeaderFieldName
hAMPCacheTransform = "AMP-Cache-Transform"

-- | 'Apply-To-Redirect-Ref' HTTP Header
-- Permanent: [RFC 4437: Web Distributed Authoring and Versioning (WebDAV) Redirect Reference Resources](https://datatracker.ietf.org/doc/html/rfc4437)
hApplyToRedirectRef :: HeaderFieldName
hApplyToRedirectRef = "Apply-To-Redirect-Ref"

-- | 'Authentication-Control' HTTP Header
-- Permanent: [RFC 8053, Section 4: HTTP Authentication Extensions for Interactive Clients](https://datatracker.ietf.org/doc/html/rfc8053#section-4)
hAuthenticationControl :: HeaderFieldName
hAuthenticationControl = "Authentication-Control"

-- | 'Authentication-Info' HTTP Header
-- Permanent: [RFC9110, Section 11.6.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.6.3)
hAuthenticationInfo :: HeaderFieldName
hAuthenticationInfo = "Authentication-Info"

-- | 'Authorization' HTTP Header
-- Permanent: [RFC9110, Section 11.6.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.6.2)
hAuthorization :: HeaderFieldName
hAuthorization = "Authorization"

-- | 'Cache-Control' HTTP Header
-- Permanent: [RFC9111, Section 5.2](https://datatracker.ietf.org/doc/html/rfc9111#section-5.2)
hCacheControl :: HeaderFieldName
hCacheControl = "Cache-Control"

-- | 'Cache-Status' HTTP Header
-- Permanent: [RFC9211: The Cache-Status HTTP Response Header Field](https://datatracker.ietf.org/doc/html/rfc9211)
hCacheStatus :: HeaderFieldName
hCacheStatus = "Cache-Status"

-- | 'Cal-Managed-ID' HTTP Header
-- Permanent: [RFC 8607, Section 5.1: Calendaring Extensions to WebDAV (CalDAV): Managed Attachments](https://datatracker.ietf.org/doc/html/rfc8607#section-5.1)
hCalManagedID :: HeaderFieldName
hCalManagedID = "Cal-Managed-ID"

-- | 'CalDAV-Timezones' HTTP Header
-- Permanent: [RFC 7809, Section 7.1: Calendaring Extensions to WebDAV (CalDAV): Time Zones by Reference](https://datatracker.ietf.org/doc/html/rfc7809#section-7.1)
hCalDAVTimezones :: HeaderFieldName
hCalDAVTimezones = "CalDAV-Timezones"

-- | 'Capsule-Protocol' HTTP Header
-- Permanent: [RFC9297](https://datatracker.ietf.org/doc/html/rfc9297)
hCapsuleProtocol :: HeaderFieldName
hCapsuleProtocol = "Capsule-Protocol"

-- | 'CDN-Cache-Control' HTTP Header
-- Permanent: [RFC9213: Targeted HTTP Cache Control](https://datatracker.ietf.org/doc/html/rfc9213) Cache directives targeted at content delivery networks
hCDNCacheControl :: HeaderFieldName
hCDNCacheControl = "CDN-Cache-Control"

-- | 'CDN-Loop' HTTP Header
-- Permanent: [RFC 8586: Loop Detection in Content Delivery Networks (CDNs)](https://datatracker.ietf.org/doc/html/rfc8586)
hCDNLoop :: HeaderFieldName
hCDNLoop = "CDN-Loop"

-- | 'Cert-Not-After' HTTP Header
-- Permanent: [RFC 8739, Section 3.3: Support for Short-Term, Automatically Renewed (STAR) Certificates in the Automated Certificate Management Environment (ACME)](https://datatracker.ietf.org/doc/html/rfc8739#section-3.3)
hCertNotAfter :: HeaderFieldName
hCertNotAfter = "Cert-Not-After"

-- | 'Cert-Not-Before' HTTP Header
-- Permanent: [RFC 8739, Section 3.3: Support for Short-Term, Automatically Renewed (STAR) Certificates in the Automated Certificate Management Environment (ACME)](https://datatracker.ietf.org/doc/html/rfc8739#section-3.3)
hCertNotBefore :: HeaderFieldName
hCertNotBefore = "Cert-Not-Before"

-- | 'Clear-Site-Data' HTTP Header
-- Permanent: [Clear Site Data](https://w3c.github.io/webappsec-clear-site-data/)
hClearSiteData :: HeaderFieldName
hClearSiteData = "Clear-Site-Data"

-- | 'Client-Cert' HTTP Header
-- Permanent: [RFC9440, Section 2: Client-Cert HTTP Header Field](https://datatracker.ietf.org/doc/html/rfc9440#section-2)
hClientCert :: HeaderFieldName
hClientCert = "Client-Cert"

-- | 'Client-Cert-Chain' HTTP Header
-- Permanent: [RFC9440, Section 2: Client-Cert HTTP Header Field](https://datatracker.ietf.org/doc/html/rfc9440#section-2)
hClientCertChain :: HeaderFieldName
hClientCertChain = "Client-Cert-Chain"

-- | 'Close' HTTP Header
-- Permanent: [RFC9112, Section 9.6: HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc9112#section-9.6) (reserved)
hClose :: HeaderFieldName
hClose = "Close"

-- | 'Configuration-Context' HTTP Header
-- Provisional: [OSLC Configuration Management Version 1.0. Part 3: Configuration Specification](https://docs.oasis-open.org/oslc-core/oslc-cm/v1.0/oslc-cm-v1.0-part3-configuration-management-spec.html)
hConfigurationContext :: HeaderFieldName
hConfigurationContext = "Configuration-Context"

-- | 'Connection' HTTP Header
-- Permanent: [RFC9110, Section 7.6.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.6.1)
hConnection :: HeaderFieldName
hConnection = "Connection"

-- | 'Content-Base' HTTP Header
-- Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)[RFC 2616: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2616)
hContentBase :: HeaderFieldName
hContentBase = "Content-Base"

-- | 'Content-Digest' HTTP Header
-- Permanent: [RFC 9530, Section 2: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-2)
hContentDigest :: HeaderFieldName
hContentDigest = "Content-Digest"

-- | 'Content-Disposition' HTTP Header
-- Permanent: [RFC 6266: Use of the Content-Disposition Header Field in the Hypertext Transfer Protocol (HTTP)](https://datatracker.ietf.org/doc/html/rfc6266)
hContentDisposition :: HeaderFieldName
hContentDisposition = "Content-Disposition"

-- | 'Content-Encoding' HTTP Header
-- Permanent: [RFC9110, Section 8.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.4)
hContentEncoding :: HeaderFieldName
hContentEncoding = "Content-Encoding"

-- | 'Content-ID' HTTP Header
-- Deprecated: [The HTTP Distribution and Replication Protocol](https://www.w3.org/TR/NOTE-drp-19970625.html)
hContentID :: HeaderFieldName
hContentID = "Content-ID"

-- | 'Content-Language' HTTP Header
-- Permanent: [RFC9110, Section 8.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.5)
hContentLanguage :: HeaderFieldName
hContentLanguage = "Content-Language"

-- | 'Content-Length' HTTP Header
-- Permanent: [RFC9110, Section 8.6: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.6)
hContentLength :: HeaderFieldName
hContentLength = "Content-Length"

-- | 'Content-Location' HTTP Header
-- Permanent: [RFC9110, Section 8.7: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.7)
hContentLocation :: HeaderFieldName
hContentLocation = "Content-Location"

-- | 'Content-MD5' HTTP Header
-- Obsoleted: [RFC 2616, Section 14.15: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2616#section-14.15)[RFC 7231, Appendix B: Hypertext Transfer Protocol (HTTP/1.1): Semantics and Content](https://datatracker.ietf.org/doc/html/rfc7231#appendix-B)
hContentMD5 :: HeaderFieldName
hContentMD5 = "Content-MD5"

-- | 'Content-Range' HTTP Header
-- Permanent: [RFC9110, Section 14.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-14.4)
hContentRange :: HeaderFieldName
hContentRange = "Content-Range"

-- | 'Content-Script-Type' HTTP Header
-- Obsoleted: [HTML 4.01 Specification](https://www.w3.org/TR/html401/)
hContentScriptType :: HeaderFieldName
hContentScriptType = "Content-Script-Type"

-- | 'Content-Security-Policy' HTTP Header
-- Permanent: [Content Security Policy Level 3](https://www.w3.org/TR/CSP3/)
hContentSecurityPolicy :: HeaderFieldName
hContentSecurityPolicy = "Content-Security-Policy"

-- | 'Content-Security-Policy-Report-Only' HTTP Header
-- Permanent: [Content Security Policy Level 3](https://www.w3.org/TR/CSP3/)
hContentSecurityPolicyReportOnly :: HeaderFieldName
hContentSecurityPolicyReportOnly = "Content-Security-Policy-Report-Only"

-- | 'Content-Style-Type' HTTP Header
-- Obsoleted: [HTML 4.01 Specification](https://www.w3.org/TR/html401/)
hContentStyleType :: HeaderFieldName
hContentStyleType = "Content-Style-Type"

-- | 'Content-Type' HTTP Header
-- Permanent: [RFC9110, Section 8.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.3)
hContentType :: HeaderFieldName
hContentType = "Content-Type"

-- | 'Content-Version' HTTP Header
-- Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)
hContentVersion :: HeaderFieldName
hContentVersion = "Content-Version"

-- | 'Cookie' HTTP Header
-- Permanent: [RFC 6265: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc6265)
hCookie :: HeaderFieldName
hCookie = "Cookie"

-- | 'Cookie2' HTTP Header
-- Obsoleted: [RFC 2965: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc2965)[RFC 6265: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc6265)
hCookie2 :: HeaderFieldName
hCookie2 = "Cookie2"

-- | 'Cross-Origin-Embedder-Policy' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hCrossOriginEmbedderPolicy :: HeaderFieldName
hCrossOriginEmbedderPolicy = "Cross-Origin-Embedder-Policy"

-- | 'Cross-Origin-Embedder-Policy-Report-Only' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hCrossOriginEmbedderPolicyReportOnly :: HeaderFieldName
hCrossOriginEmbedderPolicyReportOnly = "Cross-Origin-Embedder-Policy-Report-Only"

-- | 'Cross-Origin-Opener-Policy' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hCrossOriginOpenerPolicy :: HeaderFieldName
hCrossOriginOpenerPolicy = "Cross-Origin-Opener-Policy"

-- | 'Cross-Origin-Opener-Policy-Report-Only' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hCrossOriginOpenerPolicyReportOnly :: HeaderFieldName
hCrossOriginOpenerPolicyReportOnly = "Cross-Origin-Opener-Policy-Report-Only"

-- | 'Cross-Origin-Resource-Policy' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hCrossOriginResourcePolicy :: HeaderFieldName
hCrossOriginResourcePolicy = "Cross-Origin-Resource-Policy"

-- | 'DASL' HTTP Header
-- Permanent: [RFC 5323: Web Distributed Authoring and Versioning (WebDAV) SEARCH](https://datatracker.ietf.org/doc/html/rfc5323)
hDASL :: HeaderFieldName
hDASL = "DASL"

-- | 'Date' HTTP Header
-- Permanent: [RFC9110, Section 6.6.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-6.6.1)
hDate :: HeaderFieldName
hDate = "Date"

-- | 'DAV' HTTP Header
-- Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)
hDAV :: HeaderFieldName
hDAV = "DAV"

-- | 'Default-Style' HTTP Header
-- Obsoleted: [HTML 4.01 Specification](https://www.w3.org/TR/html401/)
hDefaultStyle :: HeaderFieldName
hDefaultStyle = "Default-Style"

-- | 'Delta-Base' HTTP Header
-- Permanent: [RFC 3229: Delta encoding in HTTP](https://datatracker.ietf.org/doc/html/rfc3229)
hDeltaBase :: HeaderFieldName
hDeltaBase = "Delta-Base"

-- | 'Depth' HTTP Header
-- Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)
hDepth :: HeaderFieldName
hDepth = "Depth"

-- | 'Derived-From' HTTP Header
-- Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)
hDerivedFrom :: HeaderFieldName
hDerivedFrom = "Derived-From"

-- | 'Destination' HTTP Header
-- Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)
hDestination :: HeaderFieldName
hDestination = "Destination"

-- | 'Differential-ID' HTTP Header
-- Deprecated: [The HTTP Distribution and Replication Protocol](https://www.w3.org/TR/NOTE-drp-19970625.html)
hDifferentialID :: HeaderFieldName
hDifferentialID = "Differential-ID"

-- | 'Digest' HTTP Header
-- Obsoleted: [RFC 3230: Instance Digests in HTTP](https://datatracker.ietf.org/doc/html/rfc3230)[RFC 9530, Section 1.3: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-1.3)
hDigest :: HeaderFieldName
hDigest = "Digest"

-- | 'DPoP' HTTP Header
-- Permanent: [RFC9449: OAuth 2.0 Demonstrating Proof of Possession (DPoP)](https://datatracker.ietf.org/doc/html/rfc9449)
hDPoP :: HeaderFieldName
hDPoP = "DPoP"

-- | 'DPoP-Nonce' HTTP Header
-- Permanent: [RFC9449: OAuth 2.0 Demonstrating Proof of Possession (DPoP)](https://datatracker.ietf.org/doc/html/rfc9449)
hDPoPNonce :: HeaderFieldName
hDPoPNonce = "DPoP-Nonce"

-- | 'Early-Data' HTTP Header
-- Permanent: [RFC 8470: Using Early Data in HTTP](https://datatracker.ietf.org/doc/html/rfc8470)
hEarlyData :: HeaderFieldName
hEarlyData = "Early-Data"

-- | 'EDIINT-Features' HTTP Header
-- Provisional: [RFC 6017: Electronic Data Interchange - Internet Integration (EDIINT) Features Header Field](https://datatracker.ietf.org/doc/html/rfc6017)
hEDIINTFeatures :: HeaderFieldName
hEDIINTFeatures = "EDIINT-Features"

-- | 'ETag' HTTP Header
-- Permanent: [RFC9110, Section 8.8.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.8.3)
hETag :: HeaderFieldName
hETag = "ETag"

-- | 'Expect' HTTP Header
-- Permanent: [RFC9110, Section 10.1.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.1)
hExpect :: HeaderFieldName
hExpect = "Expect"

-- | 'Expect-CT' HTTP Header
-- Deprecated: [RFC9163: Expect-CT Extension for HTTP](https://datatracker.ietf.org/doc/html/rfc9163)[IESG](https://www.ietf.org/)[HTTPBIS](https://datatracker.ietf.org/doc/html/rfc9110)
hExpectCT :: HeaderFieldName
hExpectCT = "Expect-CT"

-- | 'Expires' HTTP Header
-- Permanent: [RFC9111, Section 5.3: HTTP Caching](https://datatracker.ietf.org/doc/html/rfc9111#section-5.3)
hExpires :: HeaderFieldName
hExpires = "Expires"

-- | 'Ext' HTTP Header
-- Obsoleted: [RFC 2774: An HTTP Extension Framework](https://datatracker.ietf.org/doc/html/rfc2774)[status-change-http-experiments-to-historic](https://www.ietf.org/)
hExt :: HeaderFieldName
hExt = "Ext"

-- | 'Forwarded' HTTP Header
-- Permanent: [RFC 7239: Forwarded HTTP Extension](https://datatracker.ietf.org/doc/html/rfc7239)
hForwarded :: HeaderFieldName
hForwarded = "Forwarded"

-- | 'From' HTTP Header
-- Permanent: [RFC9110, Section 10.1.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.2)
hFrom :: HeaderFieldName
hFrom = "From"

-- | 'GetProfile' HTTP Header
-- Obsoleted: [Implementation of OPS Over HTTP](https://www.w3.org/TR/OPS-Implementation/)
hGetProfile :: HeaderFieldName
hGetProfile = "GetProfile"

-- | 'Hobareg' HTTP Header
-- Permanent: [RFC 7486, Section 6.1.1: HTTP Origin-Bound Authentication (HOBA)](https://datatracker.ietf.org/doc/html/rfc7486#section-6.1.1)
hHobareg :: HeaderFieldName
hHobareg = "Hobareg"

-- | 'Host' HTTP Header
-- Permanent: [RFC9110, Section 7.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.2)
hHost :: HeaderFieldName
hHost = "Host"

-- | 'HTTP2-Settings' HTTP Header
-- Obsoleted: [RFC 7540, Section 3.2.1: Hypertext Transfer Protocol Version 2 (HTTP/2)](https://datatracker.ietf.org/doc/html/rfc7540#section-3.2.1) Obsolete; see Section 11.1 of [RFC9113](https://datatracker.ietf.org/doc/html/rfc9113)
hHTTP2Settings :: HeaderFieldName
hHTTP2Settings = "HTTP2-Settings"

-- | 'If' HTTP Header
-- Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)
hIf :: HeaderFieldName
hIf = "If"

-- | 'If-Match' HTTP Header
-- Permanent: [RFC9110, Section 13.1.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.1)
hIfMatch :: HeaderFieldName
hIfMatch = "If-Match"

-- | 'If-Modified-Since' HTTP Header
-- Permanent: [RFC9110, Section 13.1.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.3)
hIfModifiedSince :: HeaderFieldName
hIfModifiedSince = "If-Modified-Since"

-- | 'If-None-Match' HTTP Header
-- Permanent: [RFC9110, Section 13.1.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.2)
hIfNoneMatch :: HeaderFieldName
hIfNoneMatch = "If-None-Match"

-- | 'If-Range' HTTP Header
-- Permanent: [RFC9110, Section 13.1.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.5)
hIfRange :: HeaderFieldName
hIfRange = "If-Range"

-- | 'If-Schedule-Tag-Match' HTTP Header
-- Permanent: [ RFC 6338: Scheduling Extensions to CalDAV](https://datatracker.ietf.org/doc/html/rfc6338)
hIfScheduleTagMatch :: HeaderFieldName
hIfScheduleTagMatch = "If-Schedule-Tag-Match"

-- | 'If-Unmodified-Since' HTTP Header
-- Permanent: [RFC9110, Section 13.1.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-13.1.4)
hIfUnmodifiedSince :: HeaderFieldName
hIfUnmodifiedSince = "If-Unmodified-Since"

-- | 'IM' HTTP Header
-- Permanent: [RFC 3229: Delta encoding in HTTP](https://datatracker.ietf.org/doc/html/rfc3229)
hIM :: HeaderFieldName
hIM = "IM"

-- | 'Include-Referred-Token-Binding-ID' HTTP Header
-- Permanent: [RFC 8473: Token Binding over HTTP](https://datatracker.ietf.org/doc/html/rfc8473)
hIncludeReferredTokenBindingID :: HeaderFieldName
hIncludeReferredTokenBindingID = "Include-Referred-Token-Binding-ID"

-- | 'Isolation' HTTP Header
-- Provisional: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)
hIsolation :: HeaderFieldName
hIsolation = "Isolation"

-- | 'Keep-Alive' HTTP Header
-- Permanent: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)
hKeepAlive :: HeaderFieldName
hKeepAlive = "Keep-Alive"

-- | 'Label' HTTP Header
-- Permanent: [RFC 3253: Versioning Extensions to WebDAV: (Web Distributed Authoring and Versioning)](https://datatracker.ietf.org/doc/html/rfc3253)
hLabel :: HeaderFieldName
hLabel = "Label"

-- | 'Last-Event-ID' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hLastEventID :: HeaderFieldName
hLastEventID = "Last-Event-ID"

-- | 'Last-Modified' HTTP Header
-- Permanent: [RFC9110, Section 8.8.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-8.8.2)
hLastModified :: HeaderFieldName
hLastModified = "Last-Modified"

-- | 'Link' HTTP Header
-- Permanent: [RFC 8288: Web Linking](https://datatracker.ietf.org/doc/html/rfc8288)
hLink :: HeaderFieldName
hLink = "Link"

-- | 'Location' HTTP Header
-- Permanent: [RFC9110, Section 10.2.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.2)
hLocation :: HeaderFieldName
hLocation = "Location"

-- | 'Lock-Token' HTTP Header
-- Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)
hLockToken :: HeaderFieldName
hLockToken = "Lock-Token"

-- | 'Man' HTTP Header
-- Obsoleted: [RFC 2774: An HTTP Extension Framework](https://datatracker.ietf.org/doc/html/rfc2774)[status-change-http-experiments-to-historic](https://www.ietf.org/)
hMan :: HeaderFieldName
hMan = "Man"

-- | 'Max-Forwards' HTTP Header
-- Permanent: [RFC9110, Section 7.6.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.6.2)
hMaxForwards :: HeaderFieldName
hMaxForwards = "Max-Forwards"

-- | 'Memento-Datetime' HTTP Header
-- Permanent: [RFC 7089: HTTP Framework for Time-Based Access to Resource States -- Memento](https://datatracker.ietf.org/doc/html/rfc7089)
hMementoDatetime :: HeaderFieldName
hMementoDatetime = "Memento-Datetime"

-- | 'Meter' HTTP Header
-- Permanent: [RFC 2227: Simple Hit-Metering and Usage-Limiting for HTTP](https://datatracker.ietf.org/doc/html/rfc2227)
hMeter :: HeaderFieldName
hMeter = "Meter"

-- | 'Method-Check' HTTP Header
-- Obsoleted: [Access Control for Cross-site Requests](https://www.w3.org/TR/cors/)
hMethodCheck :: HeaderFieldName
hMethodCheck = "Method-Check"

-- | 'Method-Check-Expires' HTTP Header
-- Obsoleted: [Access Control for Cross-site Requests](https://www.w3.org/TR/cors/)
hMethodCheckExpires :: HeaderFieldName
hMethodCheckExpires = "Method-Check-Expires"

-- | 'MIME-Version' HTTP Header
-- Permanent: [RFC9112, Appendix B.1: HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc9112#appendix-B.1)
hMIMEVersion :: HeaderFieldName
hMIMEVersion = "MIME-Version"

-- | 'Negotiate' HTTP Header
-- Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)
hNegotiate :: HeaderFieldName
hNegotiate = "Negotiate"

-- | 'NEL' HTTP Header
-- Permanent: [Network Error Logging](https://www.w3.org/TR/network-error-logging/)
hNEL :: HeaderFieldName
hNEL = "NEL"

-- | 'OData-EntityId' HTTP Header
-- Permanent: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)
hODataEntityId :: HeaderFieldName
hODataEntityId = "OData-EntityId"

-- | 'OData-Isolation' HTTP Header
-- Permanent: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)
hODataIsolation :: HeaderFieldName
hODataIsolation = "OData-Isolation"

-- | 'OData-MaxVersion' HTTP Header
-- Permanent: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)
hODataMaxVersion :: HeaderFieldName
hODataMaxVersion = "OData-MaxVersion"

-- | 'OData-Version' HTTP Header
-- Permanent: [OData Version 4.01 Part 1: Protocol](https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=odata)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)
hODataVersion :: HeaderFieldName
hODataVersion = "OData-Version"

-- | 'Opt' HTTP Header
-- Obsoleted: [RFC 2774: An HTTP Extension Framework](https://datatracker.ietf.org/doc/html/rfc2774)[status-change-http-experiments-to-historic](https://www.ietf.org/)
hOpt :: HeaderFieldName
hOpt = "Opt"

-- | 'Optional-WWW-Authenticate' HTTP Header
-- Permanent: [RFC 8053, Section 3: HTTP Authentication Extensions for Interactive Clients](https://datatracker.ietf.org/doc/html/rfc8053#section-3)
hOptionalWWWAuthenticate :: HeaderFieldName
hOptionalWWWAuthenticate = "Optional-WWW-Authenticate"

-- | 'Ordering-Type' HTTP Header
-- Permanent: [RFC 3648: Web Distributed Authoring and Versioning (WebDAV) Ordered Collections Protocol](https://datatracker.ietf.org/doc/html/rfc3648)
hOrderingType :: HeaderFieldName
hOrderingType = "Ordering-Type"

-- | 'Origin' HTTP Header
-- Permanent: [RFC 6454: The Web Origin Concept](https://datatracker.ietf.org/doc/html/rfc6454)
hOrigin :: HeaderFieldName
hOrigin = "Origin"

-- | 'Origin-Agent-Cluster' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hOriginAgentCluster :: HeaderFieldName
hOriginAgentCluster = "Origin-Agent-Cluster"

-- | 'OSCORE' HTTP Header
-- Permanent: [RFC 8613, Section 11.1: Object Security for Constrained RESTful Environments (OSCORE)](https://datatracker.ietf.org/doc/html/rfc8613#section-11.1)
hOSCORE :: HeaderFieldName
hOSCORE = "OSCORE"

-- | 'OSLC-Core-Version' HTTP Header
-- Permanent: [OASIS Project Specification 01](https://www.oasis-open.org/)[OASIS](https://www.oasis-open.org/)[Chet_Ensign](https://www.oasis-open.org/people/profile/Chet_Ensign)
hOSLCCoreVersion :: HeaderFieldName
hOSLCCoreVersion = "OSLC-Core-Version"

-- | 'Overwrite' HTTP Header
-- Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)
hOverwrite :: HeaderFieldName
hOverwrite = "Overwrite"

-- | 'P3P' HTTP Header
-- Obsoleted: [The Platform for Privacy Preferences 1.0 (P3P1.0) Specification](https://www.w3.org/TR/P3P/)
hP3P :: HeaderFieldName
hP3P = "P3P"

-- | 'PEP' HTTP Header
-- Obsoleted: [PEP - an Extension Mechanism for HTTP](https://www.w3.org/TR/pep/)
hPEP :: HeaderFieldName
hPEP = "PEP"

-- | 'PEP-Info' HTTP Header
-- Obsoleted: [PEP - an Extension Mechanism for HTTP](https://www.w3.org/TR/pep/)
hPEPInfo :: HeaderFieldName
hPEPInfo = "PEP-Info"

-- | 'Permissions-Policy' HTTP Header
-- Provisional: [Permissions Policy](https://www.w3.org/TR/permissions-policy-1/)
hPermissionsPolicy :: HeaderFieldName
hPermissionsPolicy = "Permissions-Policy"

-- | 'PICS-Label' HTTP Header
-- Obsoleted: [PICS Label Distribution Label Syntax and Communication Protocols](https://www.w3.org/TR/PICS-labels/)
hPICSLabel :: HeaderFieldName
hPICSLabel = "PICS-Label"

-- | 'Ping-From' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hPingFrom :: HeaderFieldName
hPingFrom = "Ping-From"

-- | 'Ping-To' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hPingTo :: HeaderFieldName
hPingTo = "Ping-To"

-- | 'Position' HTTP Header
-- Permanent: [RFC 3648: Web Distributed Authoring and Versioning (WebDAV) Ordered Collections Protocol](https://datatracker.ietf.org/doc/html/rfc3648)
hPosition :: HeaderFieldName
hPosition = "Position"

-- | 'Pragma' HTTP Header
-- Deprecated: [RFC9111, Section 5.4: HTTP Caching](https://datatracker.ietf.org/doc/html/rfc9111#section-5.4)
hPragma :: HeaderFieldName
hPragma = "Pragma"

-- | 'Prefer' HTTP Header
-- Permanent: [RFC 7240: Prefer Header for HTTP](https://datatracker.ietf.org/doc/html/rfc7240)
hPrefer :: HeaderFieldName
hPrefer = "Prefer"

-- | 'Preference-Applied' HTTP Header
-- Permanent: [RFC 7240: Prefer Header for HTTP](https://datatracker.ietf.org/doc/html/rfc7240)
hPreferenceApplied :: HeaderFieldName
hPreferenceApplied = "Preference-Applied"

-- | 'Priority' HTTP Header
-- Permanent: [RFC9218: Extensible Prioritization Scheme for HTTP](https://datatracker.ietf.org/doc/html/rfc9218)
hPriority :: HeaderFieldName
hPriority = "Priority"

-- | 'ProfileObject' HTTP Header
-- Obsoleted: [Implementation of OPS Over HTTP](https://www.w3.org/TR/OPS-Implementation/)
hProfileObject :: HeaderFieldName
hProfileObject = "ProfileObject"

-- | 'Protocol' HTTP Header
-- Obsoleted: [PICS Label Distribution Label Syntax and Communication Protocols](https://www.w3.org/TR/PICS-labels/)
hProtocol :: HeaderFieldName
hProtocol = "Protocol"

-- | 'Protocol-Info' HTTP Header
-- Deprecated: [White Paper: Joint Electronic Payment Initiative](https://www.electronic-payments-initiative.org/)
hProtocolInfo :: HeaderFieldName
hProtocolInfo = "Protocol-Info"

-- | 'Protocol-Query' HTTP Header
-- Deprecated: [White Paper: Joint Electronic Payment Initiative](https://www.electronic-payments-initiative.org/)
hProtocolQuery :: HeaderFieldName
hProtocolQuery = "Protocol-Query"

-- | 'Protocol-Request' HTTP Header
-- Obsoleted: [PICS Label Distribution Label Syntax and Communication Protocols](https://www.w3.org/TR/PICS-labels/)
hProtocolRequest :: HeaderFieldName
hProtocolRequest = "Protocol-Request"

-- | 'Proxy-Authenticate' HTTP Header
-- Permanent: [RFC9110, Section 11.7.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.7.1)
hProxyAuthenticate :: HeaderFieldName
hProxyAuthenticate = "Proxy-Authenticate"

-- | 'Proxy-Authentication-Info' HTTP Header
-- Permanent: [RFC9110, Section 11.7.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.7.3)
hProxyAuthenticationInfo :: HeaderFieldName
hProxyAuthenticationInfo = "Proxy-Authentication-Info"

-- | 'Proxy-Authorization' HTTP Header
-- Permanent: [RFC9110, Section 11.7.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.7.2)
hProxyAuthorization :: HeaderFieldName
hProxyAuthorization = "Proxy-Authorization"

-- | 'Proxy-Features' HTTP Header
-- Obsoleted: [Notification for Proxy Caches](https://www.w3.org/TR/proxy-features/)
hProxyFeatures :: HeaderFieldName
hProxyFeatures = "Proxy-Features"

-- | 'Proxy-Instruction' HTTP Header
-- Obsoleted: [Notification for Proxy Caches](https://www.w3.org/TR/proxy-features/)
hProxyInstruction :: HeaderFieldName
hProxyInstruction = "Proxy-Instruction"

-- | 'Proxy-Status' HTTP Header
-- Permanent: [RFC9209: The Proxy-Status HTTP Response Header Field](https://datatracker.ietf.org/doc/html/rfc9209)
hProxyStatus :: HeaderFieldName
hProxyStatus = "Proxy-Status"

-- | 'Public' HTTP Header
-- Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)
hPublicHeader :: HeaderFieldName
hPublicHeader = "Public"

-- | 'Public-Key-Pins' HTTP Header
-- Permanent: [RFC 7469: Public Key Pinning Extension for HTTP](https://datatracker.ietf.org/doc/html/rfc7469)
hPublicKeyPins :: HeaderFieldName
hPublicKeyPins = "Public-Key-Pins"

-- | 'Public-Key-Pins-Report-Only' HTTP Header
-- Permanent: [RFC 7469: Public Key Pinning Extension for HTTP](https://datatracker.ietf.org/doc/html/rfc7469)
hPublicKeyPinsReportOnly :: HeaderFieldName
hPublicKeyPinsReportOnly = "Public-Key-Pins-Report-Only"

-- | 'Range' HTTP Header
-- Permanent: [RFC9110, Section 14.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-14.2)
hRange :: HeaderFieldName
hRange = "Range"

-- | 'Redirect-Ref' HTTP Header
-- Permanent: [RFC 4437: Web Distributed Authoring and Versioning (WebDAV) Redirect Reference Resources](https://datatracker.ietf.org/doc/html/rfc4437)
hRedirectRef :: HeaderFieldName
hRedirectRef = "Redirect-Ref"

-- | 'Referer' HTTP Header
-- Permanent: [RFC9110, Section 10.1.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.3)
hReferer :: HeaderFieldName
hReferer = "Referer"

-- | 'Referer-Root' HTTP Header
-- Obsoleted: [Access Control for Cross-site Requests](https://www.w3.org/TR/cors/)
hRefererRoot :: HeaderFieldName
hRefererRoot = "Referer-Root"

-- | 'Refresh' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hRefresh :: HeaderFieldName
hRefresh = "Refresh"

-- | 'Repeatability-Client-ID' HTTP Header
-- Provisional: [Repeatable Requests Version 1.0](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_RepeatabilityClientID)
hRepeatabilityClientID :: HeaderFieldName
hRepeatabilityClientID = "Repeatability-Client-ID"

-- | 'Repeatability-First-Sent' HTTP Header
-- Provisional: [Repeatable Requests Version 1.0](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_RepeatabilityFirstSent)
hRepeatabilityFirstSent :: HeaderFieldName
hRepeatabilityFirstSent = "Repeatability-First-Sent"

-- | 'Repeatability-Request-ID' HTTP Header
-- Provisional: [Repeatable Requests Version 1.0](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_RepeatabilityRequestID)
hRepeatabilityRequestID :: HeaderFieldName
hRepeatabilityRequestID = "Repeatability-Request-ID"

-- | 'Repeatability-Result' HTTP Header
-- Provisional: [Repeatable Requests Version 1.0](https://docs.oasis-open.org/odata/repeatable-requests/v1.0/cs01/repeatable-requests-v1.0-cs01.html#sec_RepeatabilityResult)
hRepeatabilityResult :: HeaderFieldName
hRepeatabilityResult = "Repeatability-Result"

-- | 'Replay-Nonce' HTTP Header
-- Permanent: [RFC 8555, Section 6.5.1: Automatic Certificate Management Environment (ACME)](https://datatracker.ietf.org/doc/html/rfc8555#section-6.5.1)
hReplayNonce :: HeaderFieldName
hReplayNonce = "Replay-Nonce"

-- | 'Reporting-Endpoints' HTTP Header
-- Provisional: [Reporting API](https://www.w3.org/TR/reporting/)
hReportingEndpoints :: HeaderFieldName
hReportingEndpoints = "Reporting-Endpoints"

-- | 'Repr-Digest' HTTP Header
-- Permanent: [RFC 9530, Section 3: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-3)
hReprDigest :: HeaderFieldName
hReprDigest = "Repr-Digest"

-- | 'Retry-After' HTTP Header
-- Permanent: [RFC9110, Section 10.2.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.3)
hRetryAfter :: HeaderFieldName
hRetryAfter = "Retry-After"

-- | 'Safe' HTTP Header
-- Obsoleted: [RFC 2310: The Safe Response Header Field](https://datatracker.ietf.org/doc/html/rfc2310)[status-change-http-experiments-to-historic](https://www.ietf.org/)
hSafe :: HeaderFieldName
hSafe = "Safe"

-- | 'Schedule-Reply' HTTP Header
-- Permanent: [RFC 6638: Scheduling Extensions to CalDAV](https://datatracker.ietf.org/doc/html/rfc6638)
hScheduleReply :: HeaderFieldName
hScheduleReply = "Schedule-Reply"

-- | 'Schedule-Tag' HTTP Header
-- Permanent: [RFC 6338: Scheduling Extensions to CalDAV](https://datatracker.ietf.org/doc/html/rfc6338)
hScheduleTag :: HeaderFieldName
hScheduleTag = "Schedule-Tag"

-- | 'Sec-GPC' HTTP Header
-- Provisional: [Global Privacy Control (GPC)](https://globalprivacycontrol.org/)
hSecGPC :: HeaderFieldName
hSecGPC = "Sec-GPC"

-- | 'Sec-Purpose' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/) Intended to replace the (not registered) Purpose and x-moz headers.
hSecPurpose :: HeaderFieldName
hSecPurpose = "Sec-Purpose"

-- | 'Sec-Token-Binding' HTTP Header
-- Permanent: [RFC 8473: Token Binding over HTTP](https://datatracker.ietf.org/doc/html/rfc8473)
hSecTokenBinding :: HeaderFieldName
hSecTokenBinding = "Sec-Token-Binding"

-- | 'Sec-WebSocket-Accept' HTTP Header
-- Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)
hSecWebSocketAccept :: HeaderFieldName
hSecWebSocketAccept = "Sec-WebSocket-Accept"

-- | 'Sec-WebSocket-Extensions' HTTP Header
-- Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)
hSecWebSocketExtensions :: HeaderFieldName
hSecWebSocketExtensions = "Sec-WebSocket-Extensions"

-- | 'Sec-WebSocket-Key' HTTP Header
-- Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)
hSecWebSocketKey :: HeaderFieldName
hSecWebSocketKey = "Sec-WebSocket-Key"

-- | 'Sec-WebSocket-Protocol' HTTP Header
-- Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)
hSecWebSocketProtocol :: HeaderFieldName
hSecWebSocketProtocol = "Sec-WebSocket-Protocol"

-- | 'Sec-WebSocket-Version' HTTP Header
-- Permanent: [RFC 6455: The WebSocket Protocol](https://datatracker.ietf.org/doc/html/rfc6455)
hSecWebSocketVersion :: HeaderFieldName
hSecWebSocketVersion = "Sec-WebSocket-Version"

-- | 'Security-Scheme' HTTP Header
-- Obsoleted: [RFC 2660: The Secure HyperText Transfer Protocol](https://datatracker.ietf.org/doc/html/rfc2660)[status-change-http-experiments-to-historic](https://www.ietf.org/)
hSecurityScheme :: HeaderFieldName
hSecurityScheme = "Security-Scheme"

-- | 'Server' HTTP Header
-- Permanent: [RFC9110, Section 10.2.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.2.4)
hServer :: HeaderFieldName
hServer = "Server"

-- | 'Server-Timing' HTTP Header
-- Permanent: [Server Timing](https://www.w3.org/TR/server-timing/)
hServerTiming :: HeaderFieldName
hServerTiming = "Server-Timing"

-- | 'Set-Cookie' HTTP Header
-- Permanent: [RFC 6265: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc6265)
hSetCookie :: HeaderFieldName
hSetCookie = "Set-Cookie"

-- | 'Set-Cookie2' HTTP Header
-- Obsoleted: [RFC 2965: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc2965)[RFC 6265: HTTP State Management Mechanism](https://datatracker.ietf.org/doc/html/rfc6265)
hSetCookie2 :: HeaderFieldName
hSetCookie2 = "Set-Cookie2"

-- | 'SetProfile' HTTP Header
-- Obsoleted: [Implementation of OPS Over HTTP](https://www.w3.org/TR/OPS-Implementation/)
hSetProfile :: HeaderFieldName
hSetProfile = "SetProfile"

-- | 'Signature' HTTP Header
-- Permanent: [RFC 9421, Section 4.2: HTTP Message Signatures](https://datatracker.ietf.org/doc/html/rfc9421#section-4.2)
hSignature :: HeaderFieldName
hSignature = "Signature"

-- | 'Signature-Input' HTTP Header
-- Permanent: [RFC 9421, Section 4.1: HTTP Message Signatures](https://datatracker.ietf.org/doc/html/rfc9421#section-4.1)
hSignatureInput :: HeaderFieldName
hSignatureInput = "Signature-Input"

-- | 'SLUG' HTTP Header
-- Permanent: [RFC 5023: The Atom Publishing Protocol](https://datatracker.ietf.org/doc/html/rfc5023)
hSlug :: HeaderFieldName
hSlug = "SLUG"

-- | 'SoapAction' HTTP Header
-- Permanent: [Simple Object Access Protocol (SOAP) 1.1](https://www.w3.org/TR/soap/)
hSoapAction :: HeaderFieldName
hSoapAction = "SoapAction"

-- | 'Status-URI' HTTP Header
-- Permanent: [RFC 2518: HTTP Extensions for Distributed Authoring -- WEBDAV](https://datatracker.ietf.org/doc/html/rfc2518)
hStatusURI :: HeaderFieldName
hStatusURI = "Status-URI"

-- | 'Strict-Transport-Security' HTTP Header
-- Permanent: [RFC 6797: HTTP Strict Transport Security (HSTS)](https://datatracker.ietf.org/doc/html/rfc6797)
hStrictTransportSecurity :: HeaderFieldName
hStrictTransportSecurity = "Strict-Transport-Security"

-- | 'Sunset' HTTP Header
-- Permanent: [RFC 8594: The Sunset HTTP Header Field](https://datatracker.ietf.org/doc/html/rfc8594)
hSunset :: HeaderFieldName
hSunset = "Sunset"

-- | 'Surrogate-Capability' HTTP Header
-- Provisional: [Edge Architecture Specification](https://www.edge-spec.org/)
hSurrogateCapability :: HeaderFieldName
hSurrogateCapability = "Surrogate-Capability"

-- | 'Surrogate-Control' HTTP Header
-- Provisional: [Edge Architecture Specification](https://www.edge-spec.org/)
hSurrogateControl :: HeaderFieldName
hSurrogateControl = "Surrogate-Control"

-- | 'TCN' HTTP Header
-- Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)
hTCN :: HeaderFieldName
hTCN = "TCN"

-- | 'TE' HTTP Header
-- Permanent: [RFC9110, Section 10.1.4: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.4)
hTE :: HeaderFieldName
hTE = "TE"

-- | 'Timeout' HTTP Header
-- Permanent: [RFC 4918: HTTP Extensions for Web Distributed Authoring and Versioning (WebDAV)](https://datatracker.ietf.org/doc/html/rfc4918)
hTimeout :: HeaderFieldName
hTimeout = "Timeout"

-- | 'Timing-Allow-Origin' HTTP Header
-- Provisional: [Resource Timing Level 1](https://www.w3.org/TR/resource-timing-1/)
hTimingAllowOrigin :: HeaderFieldName
hTimingAllowOrigin = "Timing-Allow-Origin"

-- | 'Topic' HTTP Header
-- Permanent: [RFC 8030, Section 5.4: Generic Event Delivery Using HTTP Push](https://datatracker.ietf.org/doc/html/rfc8030#section-5.4)
hTopic :: HeaderFieldName
hTopic = "Topic"

-- | 'Traceparent' HTTP Header
-- Permanent: [Trace Context](https://www.w3.org/TR/trace-context/)
hTraceparent :: HeaderFieldName
hTraceparent = "Traceparent"

-- | 'Tracestate' HTTP Header
-- Permanent: [Trace Context](https://www.w3.org/TR/trace-context/)
hTracestate :: HeaderFieldName
hTracestate = "Tracestate"

-- | 'Trailer' HTTP Header
-- Permanent: [RFC9110, Section 6.6.2: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-6.6.2)
hTrailer :: HeaderFieldName
hTrailer = "Trailer"

-- | 'Transfer-Encoding' HTTP Header
-- Permanent: [RFC9112, Section 6.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9112#section-6.1)
hTransferEncoding :: HeaderFieldName
hTransferEncoding = "Transfer-Encoding"

-- | 'TTL' HTTP Header
-- Permanent: [RFC 8030, Section 5.2: Generic Event Delivery Using HTTP Push](https://datatracker.ietf.org/doc/html/rfc8030#section-5.2)
hTTL :: HeaderFieldName
hTTL = "TTL"

-- | 'Upgrade' HTTP Header
-- Permanent: [RFC9110, Section 7.8: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.8)
hUpgrade :: HeaderFieldName
hUpgrade = "Upgrade"

-- | 'Urgency' HTTP Header
-- Permanent: [RFC 8030, Section 5.3: Generic Event Delivery Using HTTP Push](https://datatracker.ietf.org/doc/html/rfc8030#section-5.3)
hUrgency :: HeaderFieldName
hUrgency = "Urgency"

-- | 'URI' HTTP Header
-- Obsoleted: [RFC 2068: Hypertext Transfer Protocol -- HTTP/1.1](https://datatracker.ietf.org/doc/html/rfc2068)
hURI :: HeaderFieldName
hURI = "URI"

-- | 'User-Agent' HTTP Header
-- Permanent: [RFC9110, Section 10.1.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-10.1.5)
hUserAgent :: HeaderFieldName
hUserAgent = "User-Agent"

-- | 'Variant-Vary' HTTP Header
-- Permanent: [RFC 2295: Transparent Content Negotiation in HTTP](https://datatracker.ietf.org/doc/html/rfc2295)
hVariantVary :: HeaderFieldName
hVariantVary = "Variant-Vary"

-- | 'Vary' HTTP Header
-- Permanent: [RFC9110, Section 12.5.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.5)
hVary :: HeaderFieldName
hVary = "Vary"

-- | 'Via' HTTP Header
-- Permanent: [RFC9110, Section 7.6.3: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-7.6.3)
hVia :: HeaderFieldName
hVia = "Via"

-- | 'Want-Content-Digest' HTTP Header
-- Permanent: [RFC 9530, Section 4: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-4)
hWantContentDigest :: HeaderFieldName
hWantContentDigest = "Want-Content-Digest"

-- | 'Want-Digest' HTTP Header
-- Obsoleted: [RFC 3230: Instance Digests in HTTP](https://datatracker.ietf.org/doc/html/rfc3230)[RFC 9530, Section 1.3: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-1.3)
hWantDigest :: HeaderFieldName
hWantDigest = "Want-Digest"

-- | 'Want-Repr-Digest' HTTP Header
-- Permanent: [RFC 9530, Section 4: Digest Fields](https://datatracker.ietf.org/doc/html/rfc9530#section-4)
hWantReprDigest :: HeaderFieldName
hWantReprDigest = "Want-Repr-Digest"

-- | 'Warning' HTTP Header
-- Obsoleted: [RFC9111, Section 5.5: HTTP Caching](https://datatracker.ietf.org/doc/html/rfc9111#section-5.5)
hWarning :: HeaderFieldName
hWarning = "Warning"

-- | 'WWW-Authenticate' HTTP Header
-- Permanent: [RFC9110, Section 11.6.1: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-11.6.1)
hWWWAuthenticate :: HeaderFieldName
hWWWAuthenticate = "WWW-Authenticate"

-- | 'X-Content-Type-Options' HTTP Header
-- Permanent: [Fetch](https://fetch.spec.whatwg.org/)
hXContentTypeOptions :: HeaderFieldName
hXContentTypeOptions = "X-Content-Type-Options"

-- | 'X-Frame-Options' HTTP Header
-- Permanent: [HTML](https://html.spec.whatwg.org/)
hXFrameOptions :: HeaderFieldName
hXFrameOptions = "X-Frame-Options"

-- | '*' HTTP Header
-- Permanent: [RFC9110, Section 12.5.5: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110#section-12.5.5)
hWildcardHeader :: HeaderFieldName
hWildcardHeader = "*"
