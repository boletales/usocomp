#include <vector>
#include <iostream>
#include <string>
#include <regex>
#include <tuple>

using namespace std;
using ll = long long;

// MachineLang.Def の命令を表す列挙型
enum Instruction {
  Invalid,
  Nop,
  Const,
  AddI,
  Jump,
  Load,
  Store,
  Inv,
  Copy,
  IfJump,
  NotJump,
  Add,
  Sub,
  Mult,
  Shift,
  And,
  Or,
  Xor,
  Eq,
  Gt,
  Lt,
};

enum Register {
  R0,
  R1,
  R2,
  R3,
  R4,
  R5,
  R6,
  R7,
  PC
};

enum InstType {
  Inst_,
  Inst_R,
  Inst_RR,
  Inst_RRR,
  Inst_RI,
  Inst_RRI
};

enum InstType getInstType(Instruction inst) {
  switch (inst) {
    case Invalid:
    case Nop:
      return Inst_;
    case Const:
      return Inst_RI;
    case AddI:
      return Inst_RRI;
    case Jump:
      return Inst_R;
    case Load:
    case Store:
    case Inv:
    case Copy:
    case IfJump:
    case NotJump:
      return Inst_RR;
    case Add:
    case Sub:
    case Mult:
    case Shift:
    case And:
    case Or:
    case Xor:
    case Eq:
    case Gt:
    case Lt:
      return Inst_RRR;
  }
  return Inst_;
}

enum Instruction getInst(string inststr) {
  if(inststr == "nop") return Nop;
  if(inststr == "const") return Const;
  if(inststr == "addi") return AddI;
  if(inststr == "jump") return Jump;
  if(inststr == "load") return Load;
  if(inststr == "store") return Store;
  if(inststr == "inv") return Inv;
  if(inststr == "copy") return Copy;
  if(inststr == "ijmp") return IfJump;
  if(inststr == "njmp") return NotJump;
  if(inststr == "add") return Add;
  if(inststr == "sub") return Sub;
  if(inststr == "mult") return Mult;
  if(inststr == "shift") return Shift;
  if(inststr == "and") return And;
  if(inststr == "or") return Or;
  if(inststr == "xor") return Xor;
  if(inststr == "eq") return Eq;
  if(inststr == "gt") return Gt;
  if(inststr == "lt") return Lt;

  return Invalid;
}

enum Register getReg(string regstr) {
  if(regstr == "r0") return R0;
  if(regstr == "r1") return R1;
  if(regstr == "r2") return R2;
  if(regstr == "r3") return R3;
  if(regstr == "r4") return R4;
  if(regstr == "r5") return R5;
  if(regstr == "r6") return R6;
  if(regstr == "r7") return R7;
  if(regstr == "pc") return PC;

  return R0;
}

regex reg_inst_   (R"(^\s*(nop).*$)");
regex reg_inst_r  (R"(^\s*(jump)\s+(r[0-7]|pc).*$)");
regex reg_inst_rr (R"(^\s*(load|store|inv|copy|ijmp|njmp)\s+(r[0-7]|pc)\s+(r[0-7]|pc).*$)");
regex reg_inst_rrr(R"(^\s*(add|sub|mult|shift|and|or|xor|eq|gt|lt)\s+(r[0-7]|pc)\s+(r[0-7]|pc)\s+(r[0-7]|pc).*$)");
regex reg_inst_ri (R"(^\s*(const)\s+(r[0-7]|pc)\s+(-?[0-9]+).*$)");
regex reg_inst_rri(R"(^\s*(addi)\s+(r[0-7]|pc)\s+(r[0-7]|pc)\s+(-?[0-9]+).*$)");

regex reg_getinst (R"(^\s*([a-z]+).*$)");

using InstTuple = tuple<Instruction, ll, ll, ll>;

vector<InstTuple> readProgram(){
  vector<InstTuple> program;
  
  while(true) {
    string line;
    getline(cin, line);
    if(cin.eof()) break;

    Instruction inst;
    smatch m;
    if(!regex_match(line, m, reg_getinst)) continue;

    inst = getInst(m[1].str());

    if(inst == Invalid) continue;

    switch(getInstType(inst)){
      case Inst_:
        program.push_back(make_tuple(inst, 0, 0, 0));
        break;
      case Inst_R:
        if (!regex_match(line, m, reg_inst_r  )) break;
        program.push_back(make_tuple(inst, getReg(m[2].str()), 0, 0));
        break;
      case Inst_RR:
        if (!regex_match(line, m, reg_inst_rr )) break;
        program.push_back(make_tuple(inst, getReg(m[2].str()), getReg(m[3].str()), 0));
        break;
      case Inst_RRR:
        if (!regex_match(line, m, reg_inst_rrr)) break;
        program.push_back(make_tuple(inst, getReg(m[2].str()), getReg(m[3].str()), getReg(m[4].str())));
        break;
      case Inst_RI:
        if (!regex_match(line, m, reg_inst_ri )) break;
        program.push_back(make_tuple(inst, getReg(m[2].str()), stoll(m[3].str()), 0));
        break;
      case Inst_RRI:
        if (!regex_match(line, m, reg_inst_rri)) break;
        program.push_back(make_tuple(inst, getReg(m[2].str()), getReg(m[3].str()), stoll(m[4].str())));
        break;
    }

  }
  return program;
}

string printInst(InstTuple inst){
  string inststr;
  switch(get<0>(inst)){
    case Nop:
      inststr = "nop";
      break;
    case Const:
      inststr = "const";
      break;
    case AddI:
      inststr = "addi";
      break;
    case Jump:
      inststr = "jump";
      break;
    case Load:
      inststr = "load";
      break;
    case Store:
      inststr = "store";
      break;
    case Inv:
      inststr = "inv";
      break;
    case Copy:
      inststr = "copy";
      break;
    case IfJump:
      inststr = "ijmp";
      break;
    case NotJump:
      inststr = "njmp";
      break;
    case Add:
      inststr = "add";
      break;
    case Sub:
      inststr = "sub";
      break;
    case Mult:
      inststr = "mult";
      break;
    case Shift:
      inststr = "shift";
      break;
    case And:
      inststr = "and";
      break;
    case Or:
      inststr = "or";
      break;
    case Xor:
      inststr = "xor";
      break;
    case Eq:
      inststr = "eq";
      break;
    case Gt:
      inststr = "gt";
      break;
    case Lt:
      inststr = "lt";
      break;
  }

  switch(getInstType(get<0>(inst))){
    case Inst_:
      return inststr;
    case Inst_R:
      return inststr + " r" + to_string(get<1>(inst));
    case Inst_RR:
      return inststr + " r" + to_string(get<1>(inst)) + " r" + to_string(get<2>(inst));
    case Inst_RRR:
      return inststr + " r" + to_string(get<1>(inst)) + " r" + to_string(get<2>(inst)) + " r" + to_string(get<3>(inst));
    case Inst_RI:
      return inststr + " r" + to_string(get<1>(inst)) + " " + to_string(get<2>(inst));
    case Inst_RRI:
      return inststr + " r" + to_string(get<1>(inst)) + " r" + to_string(get<2>(inst)) + " " + to_string(get<3>(inst));
  }
  return "[error]";
}


int memsize = 100000;
ll largestaddr = 0;

int runProgram(vector<InstTuple> program){
  vector<ll> mem(memsize, 0);
  vector<ll> reg(9, 0);
  ll tick = 0;

  while(true){
    ll pcold = reg[PC];
    if(pcold < 0 || pcold >= program.size()) {
      if (pcold == program.size()) {
        cout << "\rtick: " << tick << "\nprogram end." << " largestaddr: " << largestaddr << ", code: " << mem[0] << endl;
        return mem[0];
      }else{
        cout << "\rtick: " << tick << "\npc out of range" << endl;
        return -1;
      }
    }
    InstTuple inst = program[pcold];

    //cout << reg[PC] << ":\t" << printInst(inst) << "\t" << reg[R3] << endl;
    reg[PC]++;

    switch (get<0>(inst)){
      case Nop:
        break;
      case Const:
        reg[get<1>(inst)] = get<2>(inst);
        break;
      case AddI:
        reg[get<1>(inst)] = reg[get<2>(inst)] + get<3>(inst);
        break;
      case Jump:
        reg[PC] = reg[get<1>(inst)];
        break;
      case Load:  
        largestaddr = max(largestaddr, reg[get<2>(inst)]);
        reg[get<1>(inst)] = mem[reg[get<2>(inst)]];
        break;
      case Store:
        largestaddr = max(largestaddr, reg[get<2>(inst)]);
        mem[reg[get<2>(inst)]] = reg[get<1>(inst)];
        break;  
      case Inv:
        reg[get<1>(inst)] = ~reg[get<2>(inst)];
        break;
      case Copy:
        reg[get<1>(inst)] = reg[get<2>(inst)];
        break;
      case IfJump:
        if(reg[get<2>(inst)] != 0) reg[PC] = reg[get<1>(inst)];
        break;
      case NotJump:
        if(reg[get<2>(inst)] == 0) reg[PC] = reg[get<1>(inst)];
        break;
      case Add:
        reg[get<1>(inst)] = reg[get<2>(inst)] + reg[get<3>(inst)];
        break;
      case Sub:
        reg[get<1>(inst)] = reg[get<2>(inst)] - reg[get<3>(inst)];
        break;
      case Mult:
        reg[get<1>(inst)] = reg[get<2>(inst)] * reg[get<3>(inst)];
        break;
      case Shift:
        reg[get<1>(inst)] = reg[get<2>(inst)] << reg[get<3>(inst)];
        break;
      case And: 
        reg[get<1>(inst)] = reg[get<2>(inst)] & reg[get<3>(inst)];
        break;
      case Or:
        reg[get<1>(inst)] = reg[get<2>(inst)] | reg[get<3>(inst)];
        break;
      case Xor:
        reg[get<1>(inst)] = reg[get<2>(inst)] ^ reg[get<3>(inst)];
        break;
      case Eq:
        reg[get<1>(inst)] = reg[get<2>(inst)] == reg[get<3>(inst)];
        break;
      case Gt:
        reg[get<1>(inst)] = reg[get<2>(inst)] > reg[get<3>(inst)];
        break;
      case Lt:
        reg[get<1>(inst)] = reg[get<2>(inst)] < reg[get<3>(inst)];
        break;
    }
    tick++;

    if(tick % 10000000 == 0) cout << "\rtick: " << tick;
  }
}

int main()
{
  vector<InstTuple> program = readProgram();
  cout << "program size: " << program.size() << endl;
  runProgram(program);
}