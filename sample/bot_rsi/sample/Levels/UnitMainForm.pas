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
  FMX.Objects,
  UnitLevelsFrame;

type
  TMainForm = class(TForm)
    ButtonAdd: TButton;
    ButtonDeleted: TButton;
    ButtonClear: TButton;
    Rectangle: TRectangle;
    procedure FormShow(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeletedClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
  private
    FLevelsFrame: TLevelsFrame;
    procedure SetInitializationFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TMainForm }


constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  SetInitializationFrame;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FLevelsFrame);
  inherited;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := '������������ �������:';
end;

procedure TMainForm.SetInitializationFrame;
begin
  FLevelsFrame := TLevelsFrame.Create(nil);
  FLevelsFrame.Parent := Rectangle;
  FLevelsFrame.Align := TAlignLayout.Client;
end;

procedure TMainForm.ButtonAddClick(Sender: TObject);
begin
  // �������� �������
end;

procedure TMainForm.ButtonDeletedClick(Sender: TObject);
begin
  // ������� �������
end;

procedure TMainForm.ButtonClearClick(Sender: TObject);
begin
  // �������� ������� �������
end;


end.
