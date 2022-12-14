import path from 'path'
import { NodeGlobalsPolyfillPlugin } from '@esbuild-plugins/node-globals-polyfill'
import { NodeModulesPolyfillPlugin } from '@esbuild-plugins/node-modules-polyfill'
import { fileURLToPath } from 'url';

const dirname = path.dirname(fileURLToPath(import.meta.url))

export const build = {
  bundle: true,
  entryPoints: [path.join(dirname, 'src', 'js', 'index.js')],
  minify: true,
  outfile: path.join(dirname, 'static', 'index.generated.js'),
  plugins: [
    NodeGlobalsPolyfillPlugin({ buffer: true }),
    NodeModulesPolyfillPlugin(),
  ],
}

export const serve = { servedir: path.join(dirname, 'static') }


