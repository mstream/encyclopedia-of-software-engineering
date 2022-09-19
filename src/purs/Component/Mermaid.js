import mermaid from 'mermaid'

mermaid.mermaidAPI.initialize({ startOnLoad: false }); 

export function renderDiagramSvgCodeImpl(elementId, diagramDef) {
  return function () {
    return new Promise((resolve,reject) => {
      console.info(diagramDef)
      const cb = svgCode => resolve(svgCode)
      try {
        mermaid.mermaidAPI.render(elementId, diagramDef, cb)
      } catch (error) {
        if (error instanceof String) {
          reject(Error(error))
        } else if (error instanceof Error) {
          reject(error)
        } else {
          reject(Error('unexpected error'))
        }
      }
    }) 
  }  
}


