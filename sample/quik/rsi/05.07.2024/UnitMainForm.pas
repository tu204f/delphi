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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit;

type
  TMainForm = class(TForm)
    LayoutMenu: TLayout;
    ButtonSetting: TButton;
    ButtonTable: TButton;
    Edit1ValueRSI: TEdit;
    EditQty: TEdit;
    LayoutClient: TLayout;
    procedure ButtonSettingClick(Sender: TObject);
    procedure ButtonTableClick(Sender: TObject);
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
  UnitQuikTableForm;

{ TMainForm }

procedure TMainForm.ButtonSettingClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.ButtonTableClick(Sender: TObject);
begin
  // ќткрываем таблицу - дл€ проверку что загружаетс€
  QuikTableForm.Show;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'xBot';
end;

end.
