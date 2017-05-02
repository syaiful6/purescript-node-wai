module Network.HTTP.Types.Status
  ( Status(..)
  , Redirection
  , status2Redirection, number2Redirection, redirection2Status
  , status2Number
  , number2Status
  , status0
  , status100, status101
  , status200, status201, status202, status203, status204, status205, status206, status207, status208, status226
  , status300, status301, status302, status303, status304, status305, status307, status308
  , status400, status401, status402, status403, status404, status405, status406, status408, status410
  , status411, status412, status413, status414, status415, status416, status417, status418, status421, status422
  , status423, status424, status425, status426, status428, status429
  , status500, status501, status502, status503, status504, status505, status506, status507, status508, status509
  , status510, status511
  ) where

import Prelude

import Data.Maybe (Maybe(..))


data Status
  = NoStatus
  | Accepted
  | AlreadyReported
  | BadGateway
  | BadRequest
  | BandwidthLimitExceeded
  | Continue
  | Created
  | ExpectationFailed
  | FailedDependency
  | Forbidden
  | Found
  | GatewayTimeout
  | Gone
  | HTTPVersionNotSupported
  | IamTeapot
  | IamUsed
  | InsufficientStorage
  | InternalServerError
  | LengthRequired
  | Locked
  | LoopDetected
  | MethodNotAllowed
  | MisdirectedRequest
  | MovedPermanently
  | MultipleChoices
  | MultiStatus
  | NetworkAuthenticationRequired
  | NoContent
  | NonAuthoritativeInformation
  | NotAcceptable
  | NotExtended
  | NotFound
  | NotImplemented
  | NotModified
  | Ok
  | PartialContent
  | PaymentRequired
  | PermanentRedirect
  | PreconditionFailed
  | PreconditionRequired
  | ProxyAuthenticationRequired
  | RequestedRangeNotSatisfiable
  | RequestEntityTooLarge
  | RequestHeaderFieldsTooLarge
  | RequestTimeout
  | RequestURITooLong
  | ResetContent
  | SeeOther
  | ServiceUnavailable
  | SwitchingProtocols
  | TemporaryRedirect
  | TooManyRequests
  | Unauthorized
  | UnorderedCollection
  | UnprocessableEntity
  | UnsupportedMediaType
  | UpgradeRequired
  | UseProxy
  | VariantAlsoNegotiates

newtype Redirection = Redirection Status

derive newtype instance eqRedirection :: Eq Redirection
derive newtype instance ordRedirection :: Ord Redirection

derive instance eqStatus :: Eq Status
derive instance ordStatus :: Ord Status

instance showStatus :: Show Status where
  show NoStatus                        = "NoStatus"
  show Accepted                        = "Accepted"
  show AlreadyReported                 = "AlreadyReported"
  show BadGateway                      = "BadGateway"
  show BadRequest                      = "BadRequest"
  show BandwidthLimitExceeded          = "BandwidthLimitExceeded"
  show Continue                        = "Continue"
  show Created                         = "Created"
  show ExpectationFailed               = "ExpectationFailed"
  show FailedDependency                = "FailedDependency"
  show Forbidden                       = "Forbidden"
  show Found                           = "Found"
  show GatewayTimeout                  = "GatewayTimeout"
  show Gone                            = "Gone"
  show HTTPVersionNotSupported         = "HTTPVersionNotSupported"
  show IamTeapot                       = "IamTeapot"
  show IamUsed                         = "IamUsed"
  show InsufficientStorage             = "InsufficientStorage"
  show InternalServerError             = "InternalServerError"
  show Locked                          = "Locked"
  show LoopDetected                    = "LoopDetected"
  show LengthRequired                  = "LengthRequired"
  show MethodNotAllowed                = "MethodNotAllowed"
  show MisdirectedRequest              = "MisdirectedRequest"
  show MovedPermanently                = "MovedPermanently"
  show MultipleChoices                 = "MultipleChoices"
  show MultiStatus                     = "MultiStatus"
  show NetworkAuthenticationRequired   = "NetworkAuthenticationRequired"
  show NoContent                       = "NoContent"
  show NonAuthoritativeInformation     = "NonAuthoritativeInformation"
  show NotAcceptable                   = "NotAcceptable"
  show NotExtended                     = "NotExtended"
  show NotFound                        = "NotFound"
  show NotImplemented                  = "NotImplemented"
  show NotModified                     = "NotModified"
  show Ok                              = "Ok"
  show PartialContent                  = "PartialContent"
  show PaymentRequired                 = "PaymentRequired"
  show PermanentRedirect               = "PermanentRedirect"
  show PreconditionFailed              = "PreconditionFailed"
  show PreconditionRequired            = "PreconditionRequired"
  show ProxyAuthenticationRequired     = "ProxyAuthenticationRequired"
  show RequestedRangeNotSatisfiable    = "RequestedRangeNotSatisfiable"
  show RequestEntityTooLarge           = "RequestEntityTooLarge"
  show RequestHeaderFieldsTooLarge     = "RequestHeaderFieldsTooLarge"
  show RequestTimeout                  = "RequestTimeout"
  show RequestURITooLong               = "RequestURITooLong"
  show ResetContent                    = "ResetContent"
  show SeeOther                        = "SeeOther"
  show ServiceUnavailable              = "ServiceUnavailable"
  show SwitchingProtocols              = "SwitchingProtocols"
  show TemporaryRedirect               = "TemporaryRedirect"
  show TooManyRequests                 = "TooManyRequests"
  show Unauthorized                    = "Unauthorized"
  show UnorderedCollection             = "UnorderedCollection"
  show UnprocessableEntity             = "UnprocessableEntity"
  show UnsupportedMediaType            = "UnsupportedMediaType"
  show UpgradeRequired                 = "UpgradeRequired"
  show UseProxy                        = "UseProxy"
  show VariantAlsoNegotiates           = "VariantAlsoNegotiates"

instance showRedirection :: Show Redirection where
  show (Redirection s) = show s

status2Redirection :: Status -> Maybe Redirection
status2Redirection MultipleChoices   = Just (Redirection MultipleChoices)
status2Redirection MovedPermanently  = Just (Redirection MovedPermanently)
status2Redirection Found             = Just (Redirection Found)
status2Redirection SeeOther          = Just (Redirection SeeOther)
status2Redirection NotModified       = Just (Redirection NotModified)
status2Redirection UseProxy          = Just (Redirection UseProxy)
status2Redirection TemporaryRedirect = Just (Redirection TemporaryRedirect)
status2Redirection PermanentRedirect = Just (Redirection PermanentRedirect)
status2Redirection _                 = Nothing

redirection2Status :: Redirection -> Status
redirection2Status (Redirection s) = s

number2Redirection :: Int -> Maybe Redirection
number2Redirection = number2Status >=> status2Redirection

status2Number :: Status -> Int
-- 0 Default status for unsent requests
status2Number NoStatus                       = 0
-- 100 Informational
status2Number Continue                       = 100
status2Number SwitchingProtocols             = 101
-- 200 Successful
status2Number Ok                             = 200
status2Number Created                        = 201
status2Number Accepted                       = 202
status2Number NonAuthoritativeInformation    = 203
status2Number NoContent                      = 204
status2Number ResetContent                   = 205
status2Number PartialContent                 = 206
status2Number MultiStatus                    = 207
status2Number AlreadyReported                = 208
status2Number IamUsed                        = 226
-- 300 Redirection
status2Number MultipleChoices                = 300
status2Number MovedPermanently               = 301
status2Number Found                          = 302
status2Number SeeOther                       = 303
status2Number NotModified                    = 304
status2Number UseProxy                       = 305
status2Number TemporaryRedirect              = 307
status2Number PermanentRedirect              = 308
-- 400 Client Error
status2Number BadRequest                     = 400
status2Number Unauthorized                   = 401
status2Number PaymentRequired                = 402
status2Number Forbidden                      = 403
status2Number NotFound                       = 404
status2Number MethodNotAllowed               = 405
status2Number NotAcceptable                  = 406
status2Number ProxyAuthenticationRequired    = 407
status2Number RequestTimeout                 = 408
status2Number Gone                           = 410
status2Number LengthRequired                 = 411
status2Number PreconditionFailed             = 412
status2Number RequestEntityTooLarge          = 413
status2Number RequestURITooLong              = 414
status2Number UnsupportedMediaType           = 415
status2Number RequestedRangeNotSatisfiable   = 416
status2Number ExpectationFailed              = 417
status2Number IamTeapot                      = 418
status2Number MisdirectedRequest             = 421
status2Number UnprocessableEntity            = 422
status2Number Locked                         = 423
status2Number FailedDependency               = 424
status2Number UnorderedCollection            = 425
status2Number UpgradeRequired                = 426
status2Number PreconditionRequired           = 428
status2Number TooManyRequests                = 429
status2Number RequestHeaderFieldsTooLarge    = 431
-- 500 Server Error
status2Number InternalServerError            = 500
status2Number NotImplemented                 = 501
status2Number BadGateway                     = 502
status2Number ServiceUnavailable             = 503
status2Number GatewayTimeout                 = 504
status2Number HTTPVersionNotSupported        = 505
status2Number VariantAlsoNegotiates          = 506
status2Number InsufficientStorage            = 507
status2Number LoopDetected                   = 508
status2Number BandwidthLimitExceeded         = 509
status2Number NotExtended                    = 510
status2Number NetworkAuthenticationRequired  = 511

number2Status :: Int -> Maybe Status
-- 0 Default status for unsent requests
number2Status 0   = Just NoStatus
-- 100 Informational
number2Status 100 = Just Continue
number2Status 101 = Just SwitchingProtocols
-- 200 Successful
number2Status 200 = Just Ok
number2Status 201 = Just Created
number2Status 202 = Just Accepted
number2Status 203 = Just NonAuthoritativeInformation
number2Status 204 = Just NoContent
number2Status 205 = Just ResetContent
number2Status 206 = Just PartialContent
number2Status 207 = Just MultiStatus
number2Status 208 = Just AlreadyReported
number2Status 226 = Just IamUsed
-- 300 Redirection
number2Status 300 = Just MultipleChoices
number2Status 301 = Just MovedPermanently
number2Status 302 = Just Found
number2Status 303 = Just SeeOther
number2Status 304 = Just NotModified
number2Status 305 = Just UseProxy
number2Status 307 = Just TemporaryRedirect
number2Status 308 = Just PermanentRedirect
-- 400 Client Error
number2Status 400 = Just BadRequest
number2Status 401 = Just Unauthorized
number2Status 402 = Just PaymentRequired
number2Status 403 = Just Forbidden
number2Status 404 = Just NotFound
number2Status 405 = Just MethodNotAllowed
number2Status 406 = Just NotAcceptable
number2Status 407 = Just ProxyAuthenticationRequired
number2Status 408 = Just RequestTimeout
number2Status 410 = Just Gone
number2Status 411 = Just LengthRequired
number2Status 412 = Just PreconditionFailed
number2Status 413 = Just RequestEntityTooLarge
number2Status 414 = Just RequestURITooLong
number2Status 415 = Just UnsupportedMediaType
number2Status 416 = Just RequestedRangeNotSatisfiable
number2Status 417 = Just ExpectationFailed
number2Status 418 = Just IamTeapot
number2Status 421 = Just MisdirectedRequest
number2Status 422 = Just UnprocessableEntity
number2Status 423 = Just Locked
number2Status 424 = Just FailedDependency
number2Status 425 = Just UnorderedCollection
number2Status 426 = Just UpgradeRequired
number2Status 428 = Just PreconditionRequired
number2Status 429 = Just TooManyRequests
number2Status 431 = Just RequestHeaderFieldsTooLarge
-- 500 Server Error
number2Status 500 = Just InternalServerError
number2Status 501 = Just NotImplemented
number2Status 502 = Just BadGateway
number2Status 503 = Just ServiceUnavailable
number2Status 504 = Just GatewayTimeout
number2Status 505 = Just HTTPVersionNotSupported
number2Status 506 = Just VariantAlsoNegotiates
number2Status 507 = Just InsufficientStorage
number2Status 508 = Just LoopDetected
number2Status 509 = Just BandwidthLimitExceeded
number2Status 510 = Just NotExtended
number2Status 511 = Just NetworkAuthenticationRequired
number2Status _   = Nothing

status0 :: Status
status0   = NoStatus

status100 :: Status
status100 = Continue

status101 :: Status
status101 = SwitchingProtocols

status200 :: Status
status200 = Ok

status201 :: Status
status201 = Created

status202 :: Status
status202 = Accepted

status203 :: Status
status203 = NonAuthoritativeInformation

status204 :: Status
status204 = NoContent

status205 :: Status
status205 = ResetContent

status206 :: Status
status206 = PartialContent

status207 :: Status
status207 = MultiStatus

status208 :: Status
status208 = AlreadyReported

status226 :: Status
status226 = IamUsed

status300 :: Status
status300 = MultipleChoices

status301 :: Status
status301 = MovedPermanently

status302 :: Status
status302 = Found

status303 :: Status
status303 = SeeOther

status304 :: Status
status304 = NotModified

status305 :: Status
status305 = UseProxy

status307 :: Status
status307 = TemporaryRedirect

status308 :: Status
status308 = PermanentRedirect

status400 :: Status
status400 = BadRequest

status401 :: Status
status401 = Unauthorized

status402 :: Status
status402 = PaymentRequired

status403 :: Status
status403 = Forbidden

status404 :: Status
status404 = NotFound

status405 :: Status
status405 = MethodNotAllowed

status406 :: Status
status406 = NotAcceptable

status407 :: Status
status407 = ProxyAuthenticationRequired

status408 :: Status
status408 = RequestTimeout

status410 :: Status
status410 = Gone

status411 :: Status
status411 = LengthRequired

status412 :: Status
status412 = PreconditionFailed

status413 :: Status
status413 = RequestEntityTooLarge

status414 :: Status
status414 = RequestURITooLong

status415 :: Status
status415 = UnsupportedMediaType

status416 :: Status
status416 = RequestedRangeNotSatisfiable

status417 :: Status
status417 = ExpectationFailed

status418 :: Status
status418 = IamTeapot

status421 :: Status
status421 = MisdirectedRequest

status422 :: Status
status422 = UnprocessableEntity

status423 :: Status
status423 = Locked

status424 :: Status
status424 = FailedDependency

status425 :: Status
status425 = UnorderedCollection

status426 :: Status
status426 = UpgradeRequired

status428 :: Status
status428 = PreconditionRequired

status429 :: Status
status429 = TooManyRequests

status500 :: Status
status500 = InternalServerError

status501 :: Status
status501 = NotImplemented

status502 :: Status
status502 = BadGateway

status503 :: Status
status503 = ServiceUnavailable

status504 :: Status
status504 = GatewayTimeout

status505 :: Status
status505 = HTTPVersionNotSupported

status506 :: Status
status506 = VariantAlsoNegotiates

status507 :: Status
status507 = InsufficientStorage

status508 :: Status
status508 = LoopDetected

status509 :: Status
status509 = BandwidthLimitExceeded

status510 :: Status
status510 = NotExtended

status511 :: Status
status511 = NetworkAuthenticationRequired
