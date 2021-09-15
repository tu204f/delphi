unit Lb.ModuleFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.Create.DB,
  FMX.Memo.Types,
  FMX.ListBox,
  FMX.Edit,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Objects,
  Lb.SysUtils,
  Lb.WinFrame;

type
  TModuleFrame = class(TFrame, IWinModule)
    Layout: TLayout;
    LayoutDescription: TLayout;
    TextDescription: TText;
    MemoDescription: TMemo;
    GridPanelLayout: TGridPanelLayout;
    TextID: TText;
    EditID: TEdit;
    TextName: TText;
    EditFieldName: TEdit;
    TextSysName: TText;
    EditFieldSysName: TEdit;
  private
    FModule: TCrModule;
    procedure SetModule(const Value: TCrModule);
  private
    FStatus: TStatusFrame;
    function GetCode: WideString;
    function GetStatus: TStatusFrame;
    procedure SetStatus(const AStatus: TStatusFrame);
  protected
    procedure SetApply;
    procedure SetClose;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Module: TCrModule read FModule write SetModule;
    property Code: WideString read GetCode;
    property Status: TStatusFrame read GetStatus write SetStatus;
  end;

implementation

{$R *.fmx}

uses Lb.SysUtils.Structure, Lb.Core.Events;

{ TModuleFrame }

constructor TModuleFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Align := TAlignLayout.Client;
  FModule := nil;
end;

destructor TModuleFrame.Destroy;
begin

  inherited;
end;

procedure TModuleFrame.SetModule(const Value: TCrModule);
begin
  FModule := Value;
  if Assigned(FModule) then
  begin
    EditID.Text := FModule.ObjectKey;
    EditFieldName.Text := FModule.Name;
    EditFieldSysName.Text := FModule.SysName;
    MemoDescription.Lines.Text := FModule.Description;
  end;
end;

function TModuleFrame.GetCode: WideString;
begin
  Result := Self.ClassName;
end;

function TModuleFrame.GetStatus: TStatusFrame;
begin
  Result := FStatus;
end;

procedure TModuleFrame.SetStatus(const AStatus: TStatusFrame);
begin
  FStatus := AStatus;
  if FStatus = TStatusFrame.fsCreate then
  begin
    Self.Module := TCrModule.Create;
    with Self.Module do
    begin
      Name := '';
      SysName := '';
      Description := '';
      TimeСreation := System.SysUtils.Date + System.SysUtils.Time;
      TimeUpdate := System.SysUtils.Date + System.SysUtils.Time;
      Value := '';
    end;
  end;
end;

procedure TModuleFrame.SetApply;
begin
  // Применить изменение
  if Assigned(FModule) then
  begin
    FModule.Name := EditFieldName.Text;
    FModule.SysName := EditFieldSysName.Text;
    FModule.Description := MemoDescription.Lines.Text;
    CrModules.Add(FModule);
    Structure.SetModule(FModule);
    ApplicationEvents.SetEvent(EVENT_MODULE_TABLE_UPDATA,Self);
  end;
end;

procedure TModuleFrame.SetClose;
begin
  // Отменить заявки
end;

end.
