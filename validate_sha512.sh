#Build the OpenSSL Sha512 reference 
g++ test_sha512.cpp -lssl -lcrypto -o sha512_OpenSSL

#Add Spacing
echo " "

#Run the OpenSSL Sha512 Implementation Reference
echo "OpenSSL Sha512"
./sha512_OpenSSL sha512

#Add Spacing
echo " "

#Build dogecoin's Bitcoin Sha512 Implementation with the same block message ( 0xffffff )
cd src/crypto
g++ -I.. -I../config/  sha512.cpp  -o dogesha512

#Run dogecoin's Bitcoin Sha512 Implementation
./dogesha512

#Add Spacing
echo " "
