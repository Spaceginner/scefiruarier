use std::collections::HashMap;


macro_rules! ok {
    ($v:expr) => { $v.ok_or(()) };
}

pub struct Program(Vec<u8>);


impl Program {
    pub fn get_binary(&self) -> &[u8] {
        &self.0
    }
}


impl TryFrom<&str> for Program {
    type Error = ();

    fn try_from(code: &str) -> Result<Self, Self::Error> {
        let mut labels = HashMap::<String, u16>::new();
        let mut byte = 0;

        Ok(Self(code.lines()
            .map(str::trim)
            .map(|s| s.splitn(2, ";").next().unwrap_or_default().trim())
            .filter(|s| !s.is_empty())
            .map(|s| Token::try_from(s).expect("fuk u"))
            .map(|token| {
                match token {
                    Token::Label(label) => { labels.insert(label, byte); None },
                    Token::Instruction(instr) => Some({
                        byte += instr.clone().size();
                        match instr.clone() {
                            Instruction::Copy([reg_a, reg_b]) |
                            Instruction::CopyIfZero([reg_a, reg_b]) |
                            Instruction::CopyIfNotZero([reg_a, reg_b]) => {
                                vec![instr.into(), reg_a.into(), reg_b.into()]
                            },
                            Instruction::Move([val]) |
                            Instruction::MoveIfZero([val]) |
                            Instruction::MoveIfNotZero([val]) => {
                                let value = match val {
                                    Value::Integer(int) => int,
                                    Value::LabelReference(label) => *labels.get(&label).unwrap_or_else(|| panic!("label 404"))
                                }.to_be_bytes();
                                vec![instr.into(), value[0], value[1]]
                            },
                            instr => vec![instr.into()]
                        }
                    })
                }
            })
            .filter(Option::is_some)
            .map(Option::unwrap)
            .fold(Vec::new(), |mut program, mut instr_bytes| { program.append(&mut instr_bytes); program })))
    }
}

pub enum Token {
    Instruction(Instruction),
    Label(String),
}

impl TryFrom<&str> for Token {
    type Error = ();

    fn try_from(raw: &str) -> Result<Self, Self::Error> {
        if let Some(label) = raw.trim().strip_suffix(":") {
            Ok(Self::Label(String::from(label)))
        } else {
            Ok(Self::Instruction(Instruction::try_from(raw)?))
        }
    }
}


#[derive(Debug, Clone)]
pub enum Value {
    Integer(u16),
    LabelReference(String),
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        if let Ok(num) = value.parse() {
            Self::Integer(num)
        } else if let Some(hex) = value.strip_prefix("0x") {
            Self::Integer(u16::from_str_radix(hex, 16).expect("pls write correct hex nums or dont use 0x prefix in labels thx"))
        } else if let Some(octal) = value.strip_prefix("0o") {
            Self::Integer(u16::from_str_radix(octal, 7).expect("pls write correct octal nums or dont use 0o prefix in labels thx"))
        } else if let Some(binary) = value.strip_prefix("0b") {
            Self::Integer(u16::from_str_radix(binary, 2).expect("pls write correct binary nums or dont use 0b prefix in labels thx"))
        } else {
            Self::LabelReference(String::from(value))
        }
    }
}


#[derive(Debug, Clone)]
pub enum Instruction {
    NoOperation, Halt,

    Sum, Subtraction, Multiplication,
    BitwiseAnd, BitwiseOr, BitwiseXor,
    BitshiftRight, BitshiftLeft,
    CompareUnsigned, CompareSigned,

    Copy([Register; 2]), Move([Value; 1]),
    CopyIfZero([Register; 2]), MoveIfZero([Value; 1]),
    CopyIfNotZero([Register; 2]), MoveIfNotZero([Value; 1]),
    Put, Get,

    Test,
    Set, Unset,
    SetIfZero, UnsetIfZero,
    SetIfNotZero, UnsetIfNotZero,

    Send, Receive,
}

impl Instruction {
    pub fn size(self) -> u16 {
        match self {
            Instruction::NoOperation => 1,
            Instruction::Halt => 1,
            Instruction::Sum => 1,
            Instruction::Subtraction => 1,
            Instruction::Multiplication => 1,
            Instruction::BitwiseAnd => 1,
            Instruction::BitwiseOr => 1,
            Instruction::BitwiseXor => 1,
            Instruction::BitshiftRight => 1,
            Instruction::BitshiftLeft => 1,
            Instruction::CompareUnsigned => 1,
            Instruction::CompareSigned => 1,
            Instruction::Copy(_) => 3,
            Instruction::Move(_) => 3,
            Instruction::CopyIfZero(_) => 3,
            Instruction::MoveIfZero(_) => 3,
            Instruction::CopyIfNotZero(_) => 3,
            Instruction::MoveIfNotZero(_) => 3,
            Instruction::Put => 1,
            Instruction::Get => 1,
            Instruction::Test => 1,
            Instruction::Set => 1,
            Instruction::Unset => 1,
            Instruction::SetIfZero => 1,
            Instruction::UnsetIfZero => 1,
            Instruction::SetIfNotZero => 1,
            Instruction::UnsetIfNotZero => 1,
            Instruction::Send => 1,
            Instruction::Receive => 1,
        }
    }
}


impl From<Instruction> for u8 {
    fn from(instr: Instruction) -> Self {
        match instr {
            Instruction::NoOperation => 0b0000_0000,
            Instruction::Halt => 0b0000_0001,

            Instruction::Sum => 0b0001_0000,
            Instruction::Subtraction => 0b0001_0001,
            Instruction::Multiplication => 0b0001_0010,
            Instruction::BitwiseAnd => 0b0001_0100,
            Instruction::BitwiseOr => 0b0001_0101,
            Instruction::BitwiseXor => 0b0001_0110,
            Instruction::BitshiftRight => 0b0001_1000,
            Instruction::BitshiftLeft => 0b0001_1001,
            Instruction::CompareUnsigned => 0b0001_1100,
            Instruction::CompareSigned => 0b0001_1101,

            Instruction::Copy(_) => 0b0011_0000,
            Instruction::Move(_) => 0b0011_0001,
            Instruction::CopyIfZero(_) => 0b0011_0100,
            Instruction::CopyIfNotZero(_) => 0b0011_0101,
            Instruction::MoveIfZero(_) => 0b0011_0110,
            Instruction::MoveIfNotZero(_) => 0b0011_0111,
            Instruction::Put => 0b0011_1000,
            Instruction::Get => 0b0011_1001,

            Instruction::Test => 0b0100_0000,
            Instruction::Set => 0b0100_00010,
            Instruction::Unset => 0b0100_0011,
            Instruction::SetIfZero => 0b0100_0100,
            Instruction::UnsetIfZero => 0b0100_0101,
            Instruction::SetIfNotZero => 0b0100_0110,
            Instruction::UnsetIfNotZero => 0b0100_0111,

            Instruction::Send => 0b0101_0000,
            Instruction::Receive => 0b0101_0001,
        }
    }
}

impl TryFrom<&str> for Instruction {
    type Error = ();

    fn try_from(instr: &str) -> Result<Self, Self::Error> {
        let mut line = instr.trim().splitn(2, ' ');

        let raw_instr = line.next().unwrap_or_default();
        let args = line.next().unwrap_or_default().split(',').map(str::trim).collect::<Vec<_>>();


        match raw_instr {
            "noop" => Ok(Self::NoOperation),
            "hlt" => Ok(Self::Halt),

            "sum" => Ok(Self::Sum),
            "sub" => Ok(Self::Subtraction),
            "mul" => Ok(Self::Multiplication),
            "and" => Ok(Self::BitwiseAnd),
            "or" => Ok(Self::BitwiseOr),
            "xor" => Ok(Self::BitwiseXor),
            "bsr" => Ok(Self::BitshiftRight),
            "bsl" => Ok(Self::BitshiftLeft),
            "cmpr" => Ok(Self::CompareUnsigned),
            "cmprs" => Ok(Self::CompareSigned),

            "cp" => Ok(Self::Copy([Register::try_from(*ok!(args.get(0))?)?, Register::try_from(*ok!(args.get(1))?)?])),
            "mv" => Ok(Self::Move([Value::from(*ok!(args.get(0))?)])),
            "cpz" => Ok(Self::CopyIfZero([Register::try_from(*ok!(args.get(0))?)?, Register::try_from(*ok!(args.get(1))?)?])),
            "mvz" => Ok(Self::Move([Value::from(*ok!(args.get(0))?)])),
            "cpnz" => Ok(Self::CopyIfNotZero([Register::try_from(*ok!(args.get(0))?)?, Register::try_from(*ok!(args.get(1))?)?])),
            "mvnz" => Ok(Self::Move([Value::from(*ok!(args.get(0))?)])),
            "put" => Ok(Self::Put),
            "get" => Ok(Self::Get),

            "test" => Ok(Self::Test),
            "set" => Ok(Self::Set),
            "unset" => Ok(Self::Unset),
            "setz" => Ok(Self::SetIfZero),
            "unsetz" => Ok(Self::UnsetIfZero),
            "setnnz" => Ok(Self::SetIfNotZero),
            "unsetnz" => Ok(Self::UnsetIfNotZero),

            "send" => Ok(Self::Send),
            "rcv" => Ok(Self::Receive),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Register {
    GenericA, GenericB, GenericC,
    GenericD, GenericE, GenericF,

    InstructionPointer, FlagBank,

    OperandA, OperandB, OperandC,

    DisplayA, DisplayB,
}

impl TryFrom<&str> for Register {
    type Error = ();

    fn try_from(reg: &str) -> Result<Self, Self::Error> {
        match reg.trim() {
            "rga" => Ok(Self::GenericA),
            "rgb" => Ok(Self::GenericB),
            "rgc" => Ok(Self::GenericC),
            "rgd" => Ok(Self::GenericD),
            "rge" => Ok(Self::GenericE),
            "rgf" => Ok(Self::GenericF),

            "rip" => Ok(Self::InstructionPointer),
            "rfb" => Ok(Self::FlagBank),

            "roa" => Ok(Self::OperandA),
            "rob" => Ok(Self::OperandB),
            "roc" => Ok(Self::OperandC),

            "rda" => Ok(Self::DisplayA),
            "rdb" => Ok(Self::DisplayB),

            _ => Err(()),
        }
    }
}


impl From<Register> for u8 {
    fn from(reg: Register) -> Self {
        match reg {
            Register::GenericA => 0b00000000,
            Register::GenericB => 0b00000001,
            Register::GenericC => 0b00000010,
            Register::GenericD => 0b00000011,
            Register::GenericE => 0b00000100,
            Register::GenericF => 0b00000101,
            Register::InstructionPointer => 0b00000110,
            Register::FlagBank => 0b00000111,
            Register::OperandA => 0b00001000,
            Register::OperandB => 0b00001001,
            Register::OperandC => 0b00001010,
            Register::DisplayA => 0b00001011,
            Register::DisplayB => 0b00001100,
        }
    }
}
