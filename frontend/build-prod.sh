# exit when any command fails
set -e

npm run build

rm -rf build
mkdir build

cp -r assets build/
cp -r assets-root build/
cp index.src.html build/

mv ./build/assets/bundle.js ./build/bundle.uncompressed.js

# Compress Elm bundle
npm run uglifyjs -- "./build/bundle.uncompressed.js" \
  --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
  --output="./build/bundle.compressed1.js"

npm run uglifyjs -- "./build/bundle.compressed1.js" \
  --mangle \
  --output="./build/assets/bundle.js"

# Get the SHA1 hash of the js bundle
build_hash=$(sha1sum ./build/assets/bundle.js | awk '{print $1}')

# Rename the js bundle with hash
mv ./build/assets/bundle.js ./build/assets/bundle_${build_hash}.js

# Substitute hash in the html file
sed -E "s@\"/build/assets/bundle.js\"@\"/build/assets/bundle_${build_hash}.js\"@" build/index.src.html >build/index.tmp.html
mv build/index.tmp.html build/index.src.html

# Inline all inlineable assets
npm run inline-source -- --compress false --root ./build build/index.src.html > build/index.html

# Remove just inlined assets
rm -rf build/assets/inline
rm -rf build/bundle.uncompressed.js
rm -rf build/bundle.compressed1.js
rm -rf build/index.src.html

printf "\n\n Build successful! \n\n"
