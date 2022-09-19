export function unsafeSetInnerHtmlImpl(element, html) {
  return function () {
    element.innerHTML = html;
  }
}

