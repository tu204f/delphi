unit UnitScriptForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TScriptsForm = class(TForm)
    ListBox: TListBox;
    procedure FormShow(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScriptsForm: TScriptsForm;

implementation

{$R *.dfm}

procedure TScriptsForm.FormShow(Sender: TObject);
var
  xFileName: String;
begin
  ListBox.Items.Clear;
  xFileName := ExtractFilePath(ParamStr(0)) + 'scripts.txt';
  if FileExists(xFileName) then
    ListBox.Items.LoadFromFile(xFileName);
end;

procedure TScriptsForm.ListBoxClick(Sender: TObject);
begin
  //
end;

end.
