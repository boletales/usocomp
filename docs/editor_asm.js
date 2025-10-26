import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.52.2/+esm';
import { wireTmGrammars } from 'https://cdn.jsdelivr.net/npm/monaco-editor-textmate@4.0.0/+esm';
import { Registry } from 'https://cdn.jsdelivr.net/npm/monaco-textmate@3.0.1/+esm';
import { loadWASM } from 'https://cdn.jsdelivr.net/npm/onigasm@2.2.2/+esm';
import theme from './theme.js';

await loadWASM("https://cdn.jsdelivr.net/npm/onigasm@2.2.2/lib/onigasm.wasm");

class Editor {
  constructor() {
    this.sourcemap = [];
    this.asmdeco = [];
    this.editor_asm   = monaco.editor.create(document.getElementById('div_asm')     , {value: "", smoothScrolling:true, language: "mlang", theme: "mytheme", readOnly: false});
    this.editor_minfo = monaco.editor.create(document.getElementById('machine_info'), {value: "", smoothScrolling:true, theme: "mytheme", readOnly: true, lineNumbers: "off", minimap: {enabled: false}});
  }

  focusAsm(i){
    let line = Math.min(i+1, this.editor_asm.getModel().getLineCount());
    this.asmdeco = this.editor_asm.deltaDecorations(this.asmdeco, this.editor_asm.getDecorationsInRange(new monaco.Range(line, 1, line, 1)).map(e => {e.options.className = "sourcemap_thin"; return e}).concat([
      { range: new monaco.Range(line, 1, line, 1), options: { className: 'instexec',  isWholeLine: true} }
    ]));
    this.editor_asm.revealLineInCenter(line);
  }
  clearHighlight(){
    this.asmdeco = this.editor_asm.deltaDecorations(this.asmdeco, []);
  }

  onedit(){}

  clearInfo(){
    this.editor_minfo.getModel().setValue("");
  }
  
  setInfo(minfo){
    this.editor_minfo.getModel().setValue(minfo);
  }

  loadResultJSON(result){
    if(result.error){
      this.editor_asm.getModel().setValue(result.error);
      return;
    }

    this.editor_asm.getModel().setValue(result.asm);
  }
}

const registry = new Registry({
  getGrammarDefinition: async scopeName => {
    if(scopeName === 'source.mlang') {
      return {
        format: 'json',
        content: await (await fetch('./machinelang.tmLanguage.json')).text()
      };
    }
  }
});

monaco.editor.defineTheme('mytheme', theme);
monaco.languages.register({ id: "mlang" });


async function initTM(){
  const grammers = new Map();
  grammers.set("mlang", "source.mlang");
  await wireTmGrammars(monaco, registry, grammers);
}
await initTM();

const editor = new Editor();
export { editor };