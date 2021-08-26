unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure SetLog(S: String);
begin
  Form2.Memo1.Lines.Add(S);
end;

procedure SetParser(const ASource: TStrings; const AResult: TStrings);
var
  xLine: String;
begin
  AResult.Clear;
  xLine := '';
  var xB := False;
  for var S in ASource do
  begin
    if not S.Trim.IsEmpty then
    begin
      if Pos('<элемент xmlns="tN"',S) > 0 then
      begin
        xB := True;
        if not xLine.IsEmpty then
          AResult.Add(xLine);
        xLine := '';
        Continue;
      end;

      if not xB then
        Continue;

      var xIndexBegin := Pos('="',S);
      var xIndexEnd := Pos('"',S,xIndexBegin + 3);

      xLine := xLine + Copy(S,xIndexBegin + 2,xIndexEnd - xIndexBegin - 2) + ';';

    end;
  end;

  if not xLine.IsEmpty then
    AResult.Add(xLine);
end;

procedure SetFileOpen(const AFileName: String);
var
  xStr: TStrings;
  xRes: TStrings;
begin
  xStr := TStringList.Create;
  xRes := TStringList.Create;
  try
    var xF := ExtractFilePath(ParamStr(0)) + 'data\' + AFileName + '.xml';
    xStr.LoadFromFile(xF);
    SetParser(xStr,xRes);
    xF := ExtractFilePath(ParamStr(0)) + 'data\' + AFileName + '.csv';
    xRes.SaveToFile(xF);
  finally
    FreeAndNil(xRes);
    FreeAndNil(xStr);
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  SetFileOpen('Биоматериалы');
  SetFileOpen('Исследования');
  SetFileOpen('Лаборатории');
  SetFileOpen('Гражданство');
  SetFileOpen('Биоконтейнеры');
  SetFileOpen('ВидыОплаты');
  SetFileOpen('ЦелиИсследований');
  SetFileOpen('Диагнозы');
end;

end.
