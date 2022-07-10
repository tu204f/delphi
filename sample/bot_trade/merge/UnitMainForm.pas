unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

const
  PATH = 'd:\work\git\delphi\sample\data\sber\';

function GetFileName(const ANamePaper: String; const AYear: Integer): String;
var
  xS: String;
begin
  // SBER_100101_101231.csv
  // SBER_220101_220709.csv
  if AYear < 22 then
  begin
    xS := Format('%s_%d0101_%d1231.csv',[ANamePaper,AYear,AYear]);
    //xS := ANamePaper + '_' + IntToStr(AYear) + '0101' + '_' + IntToStr(AYear) + '1231.csv';
  end
  else
  begin
    xS := Format('%s_%d0101_%d0709.csv',[ANamePaper,AYear,AYear]);
  end;
  Result := PATH + xS;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  for var i := 10 to 22 do
  begin
    var xS := GetFileName('SBER',i);
    Memo.Lines.Add(xS);
  end;
end;

end.
