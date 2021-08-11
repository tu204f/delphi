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
  TScriptForm = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScriptForm: TScriptForm;

implementation

{$R *.dfm}

uses
  Lb.Script.QPile;

procedure TScriptForm.Button1Click(Sender: TObject);
var
  xStr: TStrings;
begin
  var xCaption := 'cap_gz';
  xStr := TStringList.Create;
  try
    var xS := GetResourceScritpQPL(xCaption,'GZU1','SPBFUT','MA_2',5,100);
    xStr.Text := xS;
    xStr.SaveToFile(xCaption + '.qpl')
  finally
    FreeAndNil(xStr);
  end;
end;

end.
