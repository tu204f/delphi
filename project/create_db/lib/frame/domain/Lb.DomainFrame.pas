unit Lb.DomainFrame;

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
  Lb.SysUtils,
  Lb.WinFrame, FMX.Memo.Types, FMX.ListBox, FMX.Edit, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Objects,
  FMX.TabControl;

type
  TDomainFrame = class(TFrame, IWinModule)
    LayoutGeneral: TLayout;
    LayoutDescription: TLayout;
    TextDescription: TText;
    MemoDescription: TMemo;
    GridPanelLayout: TGridPanelLayout;
    TextID: TText;
    EditID: TEdit;
    TextName: TText;
    EditFieldName: TEdit;
    TextFieldType: TText;
    EditSchema: TEdit;
    TabControl: TTabControl;
    TabItemGeneral: TTabItem;
    TabItemDefinition: TTabItem;
    LayoutDefinition: TLayout;
    GridPanelLayoutDefinition: TGridPanelLayout;
    Text1: TText;
    Text2: TText;
    Edit2: TEdit;
    Text3: TText;
    Edit3: TEdit;
    ComboBox1: TComboBox;
    Text4: TText;
    Switch: TSwitch;
  private
    FDomain: TCrDomain;
    procedure SetDomain(const Value: TCrDomain);
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
    property Domain: TCrDomain read FDomain write SetDomain;
    property Code: WideString read GetCode;
    property Status: TStatusFrame read GetStatus write SetStatus;
  end;

implementation

{$R *.fmx}

{ TDomainFrame }

constructor TDomainFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Align := TAlignLayout.Client;
  TabControl.ActiveTab := TabItemGeneral;
end;

destructor TDomainFrame.Destroy;
begin

  inherited Destroy;
end;

procedure TDomainFrame.SetDomain(const Value: TCrDomain);
begin
  FDomain := Value;
end;

function TDomainFrame.GetCode: WideString;
begin
  Result := Self.ClassName;
end;

function TDomainFrame.GetStatus: TStatusFrame;
begin
  Result := FStatus;
end;

procedure TDomainFrame.SetStatus(const AStatus: TStatusFrame);
begin
  FStatus := AStatus;
end;

procedure TDomainFrame.SetApply;
begin
  // Применить изменение
end;

procedure TDomainFrame.SetClose;
begin
  // Отменить заявки
end;

end.
