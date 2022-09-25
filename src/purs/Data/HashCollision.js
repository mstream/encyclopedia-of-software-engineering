import crypto from 'crypto-browserify'

const digestNames = {
  'MD5': 'md5',
  'SHA-1': 'sha1',
  'SHA-256': 'sha256',
  'SHA-512': 'sha512',
}

export function digestImpl(algorithm, inputString) {
  return [
    ...crypto
      .createHash(digestNames[algorithm])
      .update(inputString)
      .digest()
      .values()
  ].flatMap(byte => [
    ...byte.toString(2).padStart(8, 0),
  ])
}
