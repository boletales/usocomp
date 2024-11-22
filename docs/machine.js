// run asm

const instructions = [
  { name: "nop"  , args: [] },
  { name: "const", args: ["r", "v"] },
  { name: "addi" , args: ["r", "r", "v"] },
  { name: "load" , args: ["r", "r"] },
  { name: "store", args: ["r", "r"] },
  { name: "add"  , args: ["r", "r", "r"] },
  { name: "sub"  , args: ["r", "r", "r"] },
  { name: "mult" , args: ["r", "r", "r"] },
  { name: "shift", args: ["r", "r", "r"] },
  { name: "and"  , args: ["r", "r", "r"] },
  { name: "or"   , args: ["r", "r", "r"] },
  { name: "xor"  , args: ["r", "r", "r"] },
  { name: "eq"   , args: ["r", "r", "r"] },
  { name: "gt"   , args: ["r", "r", "r"] },
  { name: "lt"   , args: ["r", "r", "r"] },
  { name: "inv"  , args: ["r", "r"] },
  { name: "copy" , args: ["r", "r"] },
  { name: "jump" , args: ["r"] },
  { name: "jnz"   , args: ["r", "r"] },
  { name: "jz"  , args: ["r", "r"] }
];

const registers = [
  "pc",
  "r0",
  "r1",
  "r2",
  "r3",
  "r4",
  "r5",
  "r6"
];

function parseInst(line){
  const [inst, ...rawargs] = line.replace(/\s+/g, " ").split(" ");
  let instdata = instructions.find(i => i.name === inst);
  if (!instdata) {
    return { inst: "nop", args: [] };
  }else{
    let args = instdata.args.map((arg, i) => {
      let val = rawargs[i];
      if (arg === "r") {
        return registers.find(r => r === val);
      }else{
        return parseInt(val);
      }
    });
    return { inst, args };
  }
}

const stepLimit = 100000;

class Machine {
  constructor(inst) {
    this.memory = new Int32Array(100000);
    this.registers = {};
    for (const reg of registers) {
      this.registers[reg] = 0;
    }
    this.instructions = inst.split("\n").filter(l => l.trim().length > 0).map(parseInst);
    this.lastinst = 0;
    this.steps = 0;
  }

  step(){
    this.steps++;
    if (this.steps > stepLimit) {
      return this.prettyError("too many steps: " + this.steps);
    }

    if(this.registers.pc == this.instructions.length){
      return this.prettyError("done! (exit code: " + this.memory[0] + ")");
    } else if (this.registers.pc > this.instructions.length || this.registers.pc < 0) {
      return this.prettyError("pc out of bounds: " + this.registers.pc);
    }
    this.lastinst = this.registers.pc;
    let { inst, args } = this.instructions[this.registers.pc];
    this.registers.pc++;

    switch (inst) {
      case "nop":
        break;
      case "const":
        this.registers[args[0]] = args[1];
        break;
      case "addi":
        this.registers[args[0]] = this.registers[args[1]] + args[2];
        break;
      case "load":
        if(this.registers[args[1]] < 0 || this.registers[args[1]] >= this.memory.length){
          return this.prettyError("load out of bounds: " + this.registers[args[0]]);
        }
        this.registers[args[0]] = this.memory[this.registers[args[1]]];
        break;
      case "store":
        if(this.registers[args[1]] < 0 || this.registers[args[1]] >= this.memory.length){
          return this.prettyError("store out of bounds: " + this.registers[args[0]]);
        }
        this.memory[this.registers[args[1]]] = this.registers[args[0]];
        break;
      case "add":
        this.registers[args[0]] = this.registers[args[1]] + this.registers[args[2]];
        break;
      case "sub":
        this.registers[args[0]] = this.registers[args[1]] - this.registers[args[2]];
        break;
      case "mult":
        this.registers[args[0]] = this.registers[args[1]] * this.registers[args[2]];
        break;
      case "shift":
        this.registers[args[0]] = this.registers[args[1]] << this.registers[args[2]];
        break;
      case "and":
        this.registers[args[0]] = this.registers[args[1]] & this.registers[args[2]];
        break;
      case "or":
        this.registers[args[0]] = this.registers[args[1]] | this.registers[args[2]];
        break;
      case "xor":
        this.registers[args[0]] = this.registers[args[1]] ^ this.registers[args[2]];
        break;
      case "eq":
        this.registers[args[0]] = this.registers[args[1]] === this.registers[args[2]] ? 1 : 0;
        break;
      case "gt":
        this.registers[args[0]] = this.registers[args[1]] > this.registers[args[2]]   ? 1 : 0;
        break;
      case "lt":
        this.registers[args[0]] = this.registers[args[1]] < this.registers[args[2]]   ? 1 : 0;
        break;
      case "inv":
        this.registers[args[0]] = ~this.registers[args[1]];
        break;
      case "copy":
        this.registers[args[0]] = this.registers[args[1]];
        break;
      case "jump":
        this.registers.pc = this.registers[args[0]];
        break;
      case "jz":
        if(this.registers[args[1]] === 0){
          this.registers.pc = this.registers[args[0]];
        }
        break;
      case "jnz":
        if(this.registers[args[1]] !== 0){
          this.registers.pc = this.registers[args[0]];
        }
        break;
      default:
        return {"error": "unknown instruction: " + inst};
    }
    return this.pretty();
  }

  /*
  MLCRegPC       -> MLRegPC
  MLCRegFramePtr -> MLReg0
  MLCRegStackPtr -> MLReg1
  MLCRegX        -> MLReg2
  MLCRegY        -> MLReg3
  MLCRegZ        -> MLReg4


  stackTopAddr    = regs  VP.! fromEnum (interpretReg MLCRegStackPtr)
  oldFramePtrAddr = regs  VP.! fromEnum (interpretReg MLCRegFramePtr)

  stackFrameTexts = fromMaybe "stack frame not found" (do

          let oldStackPtrAddr = oldFramePtrAddr - 1
          let returnAddrAddr  = oldFramePtrAddr - 2
          stackBottomAddr <- (+ 1) <$> (mem VP.!? oldStackPtrAddr)

          localsaddr: ([oldFramePtrAddr + 1 .. stackTopAddr])

          oldfptr    <- mem VP.!? oldFramePtrAddr
          oldsptr    <- mem VP.!? oldStackPtrAddr
          returnaddr <- mem VP.!? returnAddrAddr

          argsaddr: ([stackBottomAddr .. returnAddrAddr - 1])
  */

  prettyStackInfo(){
    let stackTopAddr = this.registers.r1;
    let oldFramePtrAddr = this.registers.r0;
    let oldStackPtrAddr = oldFramePtrAddr - 1;
    let returnAddrAddr = oldFramePtrAddr - 2;
    if (oldStackPtrAddr < 0 || oldStackPtrAddr >= this.memory.length) {
      return {"error": "oldStackPtrAddr out of bounds: " + stackTopAddr};
    }
    if (returnAddrAddr < 0 || returnAddrAddr >= this.memory.length) {
      return {"error": "returnAddrAddr out of bounds: " + stackTopAddr};
    }
    if (stackTopAddr < 0 || stackTopAddr >= this.memory.length) {
      return {"error": "stackTopAddr out of bounds: " + stackTopAddr};
    }
    let stackBottomAddr = this.memory[oldStackPtrAddr] + 1;
    if (stackBottomAddr < 0 || stackBottomAddr >= this.memory.length) {
      return {"error": "stackBottomAddr out of bounds: " + stackBottomAddr};
    }
    let localsaddr = Array.from({length: stackTopAddr - oldFramePtrAddr - 1}, (_, i) => oldFramePtrAddr + 1 + i);
    let argsaddr   = Array.from({length: returnAddrAddr - stackBottomAddr}, (_, i) => stackBottomAddr + i);

    let oldfptr    = this.memory[oldFramePtrAddr];
    let oldsptr    = this.memory[oldStackPtrAddr];
    let returnaddr = this.memory[returnAddrAddr];

    let locals = localsaddr.map(addr => this.memory[addr]);
    let args   = argsaddr  .map(addr => this.memory[addr]);

    let text_locals = localsaddr.concat().reverse().map(addr => `${addr}: ${this.memory[addr]}`);
    
    let text_frame = [
      `${oldFramePtrAddr} (old FPtr) : ${oldfptr}`,
      `${oldStackPtrAddr} (old SPtr) : ${oldsptr}`,
      `${returnAddrAddr } (return)   : ${returnaddr}`,
    ];
    
    let text_args   = argsaddr.concat().reverse().map(addr => `${addr}: ${this.memory[addr]}`);
    
    let text_stack = [
      "=== stack top ===",
      "local vars:",
      text_locals.map(l => `  ${l}`).join("\n"),
      "",
      "frame:",
      text_frame.map(f => `  ${f}`).join("\n"),
      "",
      "args:",
      text_args.map(a => `  ${a}`).join("\n"),
      "=== stack bottom ===",
    ].join("\n");    
    return {
      pretty: text_stack,
    };
  }

  pretty(){
    let text_regs = [
      `pc: ${this.registers.pc}`,
      `r0: ${this.registers.r0}`,
      `r1: ${this.registers.r1}`,
      `r2: ${this.registers.r2}`,
      `r3: ${this.registers.r3}`,
      `r4: ${this.registers.r4}`,
    ];

    let text_mem = Array.from({length: 20}, (_, i) => {
      let addr = i * 10;
      return `${addr.toString().padEnd(3, " ")}: ${Array.from(this.memory.subarray(addr, addr + 10)).map(v => v.toString().padStart(5, " ")).join(" ")}`;
    });

    let pretty = [
      "step: " + this.steps,
      "",
      "registers:",
      text_regs.map(r => `  ${r}`).join("\n"),
      "",
      "memory:",
      text_mem.map(m => `  ${m}`).join("\n"),
    ].join("\n");

    return {
      lastinst: this.lastinst,
      regnames: registers,
      registers: this.registers,
      memory: this.memory,
      stack: this.prettyStackInfo(),
      steps: this.steps,
      pretty: pretty,
    }
  }

  prettyError(reason) {
    let p = this.pretty();
    p.error = reason;
    return p;
  }
}

export default Machine;