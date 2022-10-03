export function unsafeSetInnerHtmlImpl(element, html) {
  return function() {
    element.innerHTML = html
  }
}

export function requestAnimationFrameImpl(window, cb) {
  return function() {
    const onUpdate = timestamp => cb(timestamp)()
    return window.requestAnimationFrame(onUpdate)
  }
}

