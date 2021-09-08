unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.TreeView,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TMainForm = class(TForm)
    LayoutTree: TLayout;
    LayoutPage: TLayout;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.ApplicationVersion,
  Lb.SysUtils.Structure;

var
  localStructure: TStructure = nil;

procedure TMainForm.FormShow(Sender: TObject);
var
  xS: String;
begin
  xS := 'Cоздание базы данных (PostgreSQL): ';
  xS := xS + GetApplicationVersion;
  {$IFDEF DEBUG}
  xS := xS + ' debug';
  {$ENDIF}
  Self.Caption := xS;

  localStructure := TStructure.Create;
  localStructure.FileName := ExtractFilePath(ParamStr(0)) + 'structure.sb';
  localStructure.Open;

end;

end.
