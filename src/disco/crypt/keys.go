// Copyright (C) 2013 Mark Stahl

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package crypt

import (
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
)

// Peer keys are used to exchange data between peers. These keys are 
// generated each time each peer connects to another.  The strength
// of the key is so high because it is unknown how long two peers 
// would maintain a connection.  In the future, that the strength 
// may be decreased and a (time-based) revokation protocol will be 
// put into place.
// 
// These keys are also used to establish and identify with a broker.
// The same principle concerning strength does and may later apply.
//
func CreatePeerKeys() ([]byte, []byte, error) {
	priv, x, y, err := elliptic.GenerateKey(elliptic.P521(), rand.Reader)
	if err != nil {
		return nil, nil, err
	}

	pub := elliptic.Marshal(elliptic.P521(), x, y)
	return pub, priv, err
}

// Since connections with brokers and peers is not authenticated via
// classical methods (user name, and password) determining the 
// authenticity of a peers public key is paramount.
// 
// TODO(mjs) Add the writing of the public/private DSA keys to 
// the 'get' command.  The name of the files should be 'sign_pub.key'
// and 'sign_prv.key' respectively.
func CreateSignKeys() ([]byte, []byte, error) {
	priv, err := ecdsa.GenerateKey(elliptic.P521(), rand.Reader)
	if err != nil {
		return nil, nil, err
	}

	x, y := priv.PublicKey.X, priv.PublicKey.Y
	pub := elliptic.Marshal(elliptic.P521(), x, y)

	return pub, priv.D.Bytes(), nil
}
