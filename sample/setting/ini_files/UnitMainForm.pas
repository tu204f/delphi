unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.Setting.Values;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  with GetConfigIniFile('config') do
  begin
    Open;
    with SectionByName['sys'] do
    begin
      ValueByName['path'].AsString := ExtractFilePath(ParamStr(0));
      ValueByName['version'].AsInteger := 1;
      ValueByName['number'].AsDouble := 25/3;
    end;
    Close;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  var xS := '';
  with GetConfigIniFile('quest') do
  begin
    Open;
    with SectionByName['sys'] do
    begin
      xS := xS + ValueByName['path'].ValueString + sLineBreak;
      xS := xS + ValueByName['version'].ValueString + sLineBreak;
      xS := xS + ValueByName['number'].ValueString + sLineBreak;
    end;
  end;
  ShowMessage(xS);
end;

end.
