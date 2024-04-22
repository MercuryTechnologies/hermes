module Network.IPAddress where

data IPAddress
  = IPAddressV4
  | IPAddressV6
  | IPAddressVFuture

data IPAddressV4 = IPv4

data IPAddressV6 = IPv6
