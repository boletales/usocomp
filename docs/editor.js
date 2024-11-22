import * as monaco from 'https://cdn.jsdelivr.net/npm/monaco-editor@0.52.0/+esm';
import { wireTmGrammars } from 'https://cdn.jsdelivr.net/npm/monaco-editor-textmate@4.0.0/+esm';
import { Registry } from 'https://cdn.jsdelivr.net/npm/monaco-textmate@3.0.1/+esm';
import { loadWASM } from 'https://cdn.jsdelivr.net/npm/onigasm@2.2.5/+esm';
import theme from './theme.js';

class Editor {
  constructor() {
    this.sourcemap = [];
    this.srcdeco = [];
    this.asmdeco = [];
    this.editor_src   = monaco.editor.create(document.getElementById('div_src')     , {value: "", smoothScrolling:true, language: "slang", theme: "mytheme"});
    this.editor_asm   = monaco.editor.create(document.getElementById('div_asm')     , {value: "", smoothScrolling:true, language: "mlang", theme: "mytheme", readOnly: true});
    this.editor_sinfo = monaco.editor.create(document.getElementById('stack_info')  , {value: "", smoothScrolling:true, theme: "mytheme", readOnly: true, lineNumbers: "off", minimap: {enabled: false}});
    this.editor_minfo = monaco.editor.create(document.getElementById('machine_info'), {value: "", smoothScrolling:true, theme: "mytheme", readOnly: true, lineNumbers: "off", minimap: {enabled: false}});
    this.editing = true; // disable hover* when editing

    this.editor_src.onDidChangeCursorPosition(e => this.hoverSrc(e.position.lineNumber, e.position.column));
    this.editor_asm.onDidChangeCursorPosition(e => this.hoverAsm(e.position.lineNumber, e.position.column));

    this.editor_src.onDidChangeModelContent(e => {this.editing = true; this.onedit(); this.clearHighlight();});
  }

  isInRange(smap, line, col){
    return (smap.sourcestart.line < line || (smap.sourcestart.line === line && smap.sourcestart.col <= col)) &&
           (smap.sourceend.line > line   || (smap.sourceend.line   === line && smap.sourceend.col   >= col));
  }

  onedit(){}
  oncompile(){}

  clearInfo(){
    this.editor_sinfo.getModel().setValue("");
    this.editor_minfo.getModel().setValue("");
  }
  
  setInfo(sinfo, minfo){
    this.editor_sinfo.getModel().setValue(sinfo);
    this.editor_minfo.getModel().setValue(minfo);

    this.editor_sinfo.revealLineNearTop(this.editor_sinfo.getModel().getLineCount() - 7, 1);
  }

  loadResultJSON(result){
    if(result.error){
      this.editor_asm.getModel().setValue(result.error);
      return;
    }

    this.sourcemap = result.sourcemap;
    this.editor_src.getModel().setValue(result.src);
    this.editor_asm.getModel().setValue(result.asm);
    this.editing = false;
    this.oncompile();
  }

  hoverSrc(line, col){
    if(this.editing) return;

    let smap = this.sourcemap.find(smap => this.isInRange(smap, line, col));
    if (!smap) return;

    this.highlight(smap, false, true);
  }

  hoverAsm(line, col){
    if(this.editing) return;

    let smap = this.sourcemap.find(smap => smap.asmstart <= line && line <= smap.asmend);
    if (!smap) return;

    this.highlight(smap, true, false);
  }

  highlight(smap, scroll_src, scroll_asm){

    this.srcdeco = this.editor_src.deltaDecorations(this.srcdeco, [
      { range: new monaco.Range(smap.sourcestart.line, smap.sourcestart.col, smap.sourceend.line, smap.sourceend.col), options: { className: 'sourcemap' } }
    ]);

    this.asmdeco = this.editor_asm.deltaDecorations(this.asmdeco, [
      { range: new monaco.Range(smap.asmstart, 1, smap.asmend, 1), options: { className: 'sourcemap',  isWholeLine: true} }
    ]);

    const scrolltargetsrc = Math.floor((smap.sourcestart.line + smap.sourceend.line)/2);
    const scrolltargetasm = Math.floor((smap.asmstart + smap.asmend)/2);

    if(scroll_src) this.editor_src.revealLineInCenter(scrolltargetsrc);
    if(scroll_asm) this.editor_asm.revealLineInCenter(scrolltargetasm);
  }

  focusAsm(i){
    let line = Math.min(i+1, this.editor_asm.getModel().getLineCount());
    this.hoverAsm(line, 1);
    this.asmdeco = this.editor_asm.deltaDecorations(this.asmdeco, this.editor_asm.getDecorationsInRange(new monaco.Range(line, 1, line, 1)).map(e => {e.options.className = "sourcemap_thin"; return e}).concat([
      { range: new monaco.Range(line, 1, line, 1), options: { className: 'instexec',  isWholeLine: true} }
    ]));
    this.editor_asm.revealLineInCenter(line);
  }


  clearHighlight(){
    this.srcdeco = this.editor_src.deltaDecorations(this.srcdeco, []);
    this.asmdeco = this.editor_asm.deltaDecorations(this.asmdeco, []);
  }
}

const registry = new Registry({
  getGrammarDefinition: async scopeName => {
    if(scopeName === 'source.slang') {
      return {
        format: 'json',
        content: await (await fetch('./simplelang.tmLanguage.json')).text()
      };
    }else if(scopeName === 'source.mlang') {
      return {
        format: 'json',
        content: await (await fetch('./machinelang.tmLanguage.json')).text()
      };
    }
  }
});

monaco.editor.defineTheme('mytheme', theme);
monaco.languages.register({ id: "slang" });
monaco.languages.register({ id: "mlang" });


async function initTM(){
  await loadWASM("https://cdn.jsdelivr.net/npm/onigasm@2.2.5/lib/onigasm.wasm");
  const grammers = new Map();
  grammers.set("slang", "source.slang");
  grammers.set("mlang", "source.mlang");
  await wireTmGrammars(monaco, registry, grammers);
}
await initTM();

const editor = new Editor();

monaco.languages.registerHoverProvider('slang', {
  provideHover: function(model, position) { 
    editor.hoverSrc(position.lineNumber, position.column);
  }
});

monaco.languages.registerHoverProvider('mlang', {
  provideHover: function(model, position) { 
    editor.hoverAsm(position.lineNumber, position.column);
  }
});
export { editor };