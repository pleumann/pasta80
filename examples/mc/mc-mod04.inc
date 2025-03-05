{.PA}
{*******************************************************************}
{*  SOURCE CODE MODULE: MC-MOD04                                   *}
{*  PURPOSE:            Evaluate formulas.                         *}
{*                      Recalculate spread sheet.                  *}
{*                                                                 *}
{*  NOTE:               This module contains recursive procedures  *}
{*                      and is for computer scientists only.       *}
{*******************************************************************}

var
  Form: Boolean;

{$A-}
procedure Evaluate(var IsFormula: Boolean; { True if formula}
                   var Formula: AnyString; { Fomula to evaluate}
                   var Value: Real;  { Result of formula }
                   var ErrPos: Integer);{ Position of error }
const
  Numbers: set of Char = ['0'..'9'];
  EofLine  = ^M;

var
  Pos: Integer;    { Current position in formula                     }
  Ch: Char;        { Current character being scanned                 }
  EXY: string[3];  { Intermidiate string for conversion              }

{ Procedure NextCh returns the next character in the formula         }
{ The variable Pos contains the position ann Ch the character        }

  procedure NextCh;
  begin
    repeat
      Pos:=Pos+1;
      if Pos<=Length(Formula) then
      Ch:=Formula[Pos] else Ch:=eofline;
    until Ch<>' ';
  end  { NextCh };


  function Expression: Real;
  var
    E: Real;
    Opr: Char;

    function SimpleExpression: Real;
    var
      S: Real;
      Opr: Char;

      function Term: Real;
      var
        T: Real;

        function SignedFactor: Real;

          function Factor: Real;
          type
            StandardFunction = (fabs,fsqrt,fsqr,fsin,fcos,
            farctan,fln,flog,fexp,ffact);
            StandardFunctionList = array[StandardFunction] of string[6];

          const
            StandardFunctionNames: StandardFunctionList =('ABS','SQRT','SQR','SIN','COS',
                                                          'ARCTAN','LN','LOG','EXP','FACT');
          var
            E,EE,L:  Integer;       { intermidiate variables }
            Found:Boolean;
            F: Real;
            Sf:StandardFunction;
            OldEFY,                 { Current cell  }
            EFY,
            SumFY,
            Start:Integer;
            OldEFX,
            EFX,
            SumFX:ScreenIndex;
            CellSum: Real;

              function Fact(I: Integer): Real;
              begin
                if I > 0 then begin Fact:=I*Fact(I-1); end
                else Fact:=1;
              end  { Fact };

{.PA}
          begin { Function Factor }
            if Ch in Numbers then
            begin
              Start:=Pos;
              repeat NextCh until not (Ch in Numbers);
              if Ch='.' then repeat NextCh until not (Ch in Numbers);
              if Ch='E' then
              begin
                NextCh;
                repeat NextCh until not (Ch in Numbers);
              end;
              Val(Copy(Formula,Start,Pos-Start),F,ErrPos);
            end else
            if Ch='(' then
            begin
              NextCh;
              F:=Expression;
              if Ch=')' then NextCh else ErrPos:=Pos;
            end else
            if Ch in ['A'..'G'] then { Maybe a cell reference }
            begin
              EFX:=Ch;
              NextCh;
              if Ch in Numbers then
              begin
                F:=0;
                EXY:=Ch; NextCh;
                if Ch in Numbers then
                begin
                  EXY:=EXY+Ch;
                  NextCh;
                end;
                Val(EXY,EFY,ErrPos);
                IsFormula:=true;
                if (Constant in Screen[EFX,EFY].CellStatus) and
                not (Calculated in Screen[EFX,EFY].CellStatus) then
                begin
                  Evaluate(Form,screen[EFX,EFY].contents,f,ErrPos);
                  Screen[EFX,EFY].CellStatus:=Screen[EFX,EFY].CellStatus+[Calculated]
                end else if not (Txt in Screen[EFX,EFY].CellStatus) then
                F:=Screen[EFX,EFY].Value;
                if Ch='>' then
                begin
                  OldEFX:=EFX; OldEFY:=EFY;
                  NextCh;
                  EFX:=Ch;
                  NextCh;
                  if Ch in Numbers then
                  begin
                    EXY:=Ch;
                    NextCh;
                    if Ch in Numbers then
                    begin
                      EXY:=EXY+Ch;
                      NextCh;
                    end;
                    val(EXY,EFY,ErrPos);
                    Cellsum:=0;
                    for SumFY:=OldEFY to EFY do
                    begin
                      for SumFX:=OldEFX to EFX do
                      begin
                        F:=0;
                        if (Constant in Screen[SumFX,SumFY].CellStatus) and
                        not (Calculated in Screen[SumFX,SumFY].CellStatus) then
                        begin
                          Evaluate(Form,Screen[SumFX,SumFY].contents,f,errPos);
                          Screen[SumFX,SumFY].CellStatus:=
                          Screen[SumFX,SumFY].CellStatus+[Calculated];
                        end else if not (Txt in Screen[SumFX,SumFY].CellStatus) then
                        F:=ScrEEn[SumFX,SumFY].Value;
                        Cellsum:=Cellsum+f;
                        f:=Cellsum;
                      end;
                    end;
                  end;
                end;
              end;
            end else
            begin
              found:=false;
              for sf:=fabs to ffact do
              if not found then
              begin
                l:=Length(StandardFunctionNames[sf]);
                if copy(Formula,Pos,l)=StandardFunctionNames[sf] then
                begin
                  Pos:=Pos+l-1; NextCh;
                  F:=Factor;
                  case sf of
                    fabs:     f:=abs(f);
                    fsqrt:    f:=sqrt(f);
                    fsqr:     f:=sqr(f);
                    fsin:     f:=sin(f);
                    fcos:     f:=cos(f);
                    farctan:  f:=arctan(f);
                    fln :     f:=ln(f);
                    flog:     f:=ln(f)/ln(10);
                    fexp:     f:=exp(f);
                    ffact:    f:=fact(trunc(f));
                  end;
                  Found:=true;
                end;
              end;
              if not Found then ErrPos:=Pos;
            end;
            Factor:=F;
          end { function Factor};
{.PA}

        begin { SignedFactor }
          if Ch='-' then
          begin
            NextCh; SignedFactor:=-Factor;
          end else SignedFactor:=Factor;
        end { SignedFactor };

      begin { Term }
        T:=SignedFactor;
        while Ch='^' do
        begin
          NextCh; t:=exp(ln(t)*SignedFactor);
        end;
        Term:=t;
      end { Term };


    begin { SimpleExpression }
      s:=term;
      while Ch in ['*','/'] do
      begin
        Opr:=Ch; NextCh;
        case Opr of
          '*': s:=s*term;
          '/': s:=s/term;
        end;
      end;
      SimpleExpression:=s;
    end { SimpleExpression };

  begin { Expression }
    E:=SimpleExpression;
    while Ch in ['+','-'] do
    begin
      Opr:=Ch; NextCh;
      case Opr of
        '+': e:=e+SimpleExpression;
        '-': e:=e-SimpleExpression;
      end;
    end;
    Expression:=E;
  end { Expression };


begin { procedure Evaluate }
  if Formula[1]='.' then Formula:='0'+Formula;
  if Formula[1]='+' then delete(Formula,1,1);
  IsFormula:=false;
  Pos:=0; NextCh;
  Value:=Expression;
  if Ch=EofLine then ErrPos:=0 else ErrPos:=Pos;
end { Evaluate };

{.PA}

procedure Recalculate;
var
  RFX: ScreenIndex;
  RFY:integer;
  OldValue: real;
  Err: integer;

begin
  LowVideo;
  GotoXY(1,24); ClrEol;
  Write('Calculating..');
  for RFY:=1 to FYMax do
  begin
    for RFX:='A' to FXMax do
    begin
      with Screen[RFX,RFY] do
      begin
        if (Formula in CellStatus) then
        begin
          CellStatus:=CellStatus+[Calculated];
          OldValue:=Value;
          Evaluate(Form,Contents,Value,Err);
          if OldValue<>Value then
          begin
            GotoXY(XPos[RFX],RFY+1);
            if (DEC>=0) then Write(Value:FW:DEC)
            else Write(Value:FW);
          end;
        end;
      end;
    end;
  end;
  GotoCell(FX,FY);
end;