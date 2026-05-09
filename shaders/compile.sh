set -eux pipefail

cd "$(dirname "$0")"

slangc triangle.slang -target spirv -profile spirv_1_4 -emit-spirv-directly -fvk-use-entrypoint-name -entry vertMain -entry fragMain -o triangle.spv
