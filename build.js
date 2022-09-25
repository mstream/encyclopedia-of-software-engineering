import * as options from './options.js'
import { build } from 'esbuild'

build(options.build).catch(() => process.exit(1))
