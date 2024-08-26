unit UnitWorkTableFrame;

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
  FMX.Layouts,
  FMX.TabControl,
  FMX.Controls.Presentation,
  Lb.SysUtils,
  UnitTableTradeFrame;

type
  TWorkTableFrame = class(TFrame)
    MenuLayout: TLayout;
    TabControl: TTabControl;
    TabItemTrade: TTabItem;
    ButtonTable: TButton;
    LayoutBottom: TLayout;
    ButtonApply: TButton;
    ButtonClose: TButton;
    procedure ButtonTableClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    FMainApp: IMainApp;
    FTableTradeFrame: TTableTradeFrame;
    procedure InitFrame;
  protected
    property TableTradeFrame: TTableTradeFrame read FTableTradeFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MainApp: IMainApp write FMainApp;
  end;

implementation

{$R *.fmx}

procedure TWorkTableFrame.ButtonTableClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabItemTrade;
end;

constructor TWorkTableFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitFrame;
end;

destructor TWorkTableFrame.Destroy;
begin

  inherited;
end;

procedure TWorkTableFrame.InitFrame;
begin
  TabControl.ActiveTab := TabItemTrade;
  FTableTradeFrame := TTableTradeFrame.Create(nil);
  FTableTradeFrame.Parent := TabItemTrade;
  FTableTradeFrame.Align := TAlignLayout.Client;
end;

procedure TWorkTableFrame.ButtonApplyClick(Sender: TObject);
begin
  //
end;

procedure TWorkTableFrame.ButtonCloseClick(Sender: TObject);
begin
  FMainApp.EventCloseTabControl;
end;

end.
