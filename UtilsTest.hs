import Crypto.Utils.Keys.Signature

import Data.ByteString.Char8

seedString = pack "hellohellohellohelloǰello%$*/.õƲ"

msg = pack "Hello World"



main::IO()
main = do
    
    
    let secretKey = getSecretKey seedString
    let publicKey = getPublicKey secretKey
    
    print secretKey
    print publicKey

    let signature = sign secretKey publicKey msg 
    let result = verify publicKey msg signature

    print signature
    print result
