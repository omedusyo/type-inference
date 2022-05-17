
function makeFakeElement(theInput) {
  const fakeEle = document.createElement("div");

  const styles = theInput.style;
  Object.assign(fakeEle.style, {
    position: "absolute",
    overflow: "hidden",
    whiteSpace: "nowrap",
    visibility: "hidden",
    height: "0",

    fontFamily: styles.fontFamily,
    fontSize: styles.fontSize,
    fontStyle: styles.fontStyle,
    fontWeight: styles.fontWeight,
    letterSpacing: styles.letterSpacing,
    textTransform: styles.textTransform,

    borderLeftWidth: styles.borderLeftWidth,
    borderRightWidth: styles.borderRightWidth,
    paddingLeft: styles.paddingLeft,
    paddingRight: styles.paddingRight,
  });

  return fakeEle;
}


function setWidth(fakeEle, theInput) {
  const string = theInput.value || '';
  fakeEle.innerHTML = string;

  const fakeEleStyles = window.getComputedStyle(fakeEle);
  theInput.style.width = fakeEleStyles.width
}


class CustomInput extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });

    const input = document.createElement("input");
    const inputStyle = {
      border: "none",
      padding: "0",
      margin: "0",
      background: "none",
      outline: "none",
      minWidth: "3px",
    };

    Object.assign(input.style, inputStyle);

    const fakeEle = makeFakeElement(input);
    input.addEventListener("input", _ => {
      setWidth(fakeEle, input);
    });

    this.addEventListener("focus", _ => {
      input.focus();
    });

    input.addEventListener("input", e => {
      this.value = e.target.value;
    });

    this.shadowRoot.append(input);
    this.shadowRoot.append(fakeEle);
    setTimeout(() => {
      input.value = this.value;
      input.focus();

      input.style.fontSize = this.style.fontSize;
      fakeEle.style.fontSize = this.style.fontSize;
      setWidth(fakeEle, input);
    }, 0);
  }
}

customElements.define('input-cell', CustomInput);


