unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Memo: TMemo;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

var
  Lines: array [0..11] of String = (
			'Hi, I''''m Carter',
			'My name is rod Stewart',
			'Nice to meet you, im dixon...',
			'I am smith walker it''''s nice to meet you',
			'im Carter Stewart Smith, how are you doing?',
			'There''''s no name here',
			'Hi I''''m late for an interview',
			'I am a retail manager',
			'I''''m Dixon marshall and I have a question.',
			'Where is Rod Stewart and his dog?',
			'Hi my name is Dixon Walker, where can I buy these shoes?',
			'I am hungry but I want to see Marshall first.'
  );

  Users: array [0..6] of String = (
      'Rod',
      'Stewart',
      'Carter',
      'Dixon',
      'Marshall',
      'Smith',
      'Walker'
  );

  // представленый набор I'm, I am, my name is 
  // в тесте (должно быть еще Im : {Nice to meet you, >im< dixon})
  Prs: array [0..3] of String = (
      'im',
      'I''''m',
      'I am',
      'my name is'
  );

///<summary>Есть ли приветствие в этом сообщение</summary>
function IsPrs(S: String): Boolean;
begin
  Result := False;
  for var xS in Prs do
  begin
    var xSubStr := AnsiUpperCase(xS);
    var xStr := AnsiUpperCase(S);
    if Pos(xSubStr,xStr) > 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function GetName(const S: String): String;
var
  xStr, xS2, xR: String;
begin
  Result := '';
  if not S.IsEmpty then
  begin
    xR := '';
    // Имя может состоять из одного или нескольких слов и написано с 
    // большой или с маленькой буквы и быть из набора 
    // Значит подряд
    xStr := AnsiUpperCase(S);
    if IsPrs(xStr) then
      for xS2 in Users do
      begin
        var xSubStr := AnsiUpperCase(xS2);
        var xInd := Pos(xSubStr,xStr);



        if xInd > 1 then
        begin
          xR := xR + Copy(S,xInd,xSubStr.Length) + ' ';    
        end;
      end;
    Result := xR;
  end;
end;

procedure SetLog(const S: String; AStatusMsg: Integer);
var
  xS: String;
begin
  if Assigned(Form2) then
  begin
    case AStatusMsg of
      0: xS := '>> ';
      1: xS := '<< ';
    else
      xS := '';
    end;
    xS := xS + S;
    Form2.Memo.Lines.Add(xS);
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  (*
		foreach (string s in lines) {
			Console.WriteLine(string.Format("{0} => {1}", s, GetName(s)));
		}
  *)
  for var xS in Lines do
  begin
    SetLog(xS + '=>' + GetName(xS),-1);
  end;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  Memo.Lines.Clear;
  Edit1.Text := '';
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  var xS :=  Edit1.Text;
  SetLog(xS + '=>' + GetName(xS),-1);
end;

end.
