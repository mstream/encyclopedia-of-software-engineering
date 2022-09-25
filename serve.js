import { serve } from 'esbuild'
import * as options from './options.js'

serve(options.serve, options.build)
  .then(({ host, port }) => console.info(`Serving at ${host}:${port}`))
  .catch(() => process.exit(1))
