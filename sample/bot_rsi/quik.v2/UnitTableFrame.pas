unit UnitTableFrame;

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
  UnitTableVirtualTrade;

type
  TTableFrame = class(TFrame)
    MenuLayout: TLayout;
    TabControl: TTabControl;
    TabItemVirtualTrade: TTabItem;
    ButtonTable: TButton;
    LayoutBottom: TLayout;
    ButtonApply: TButton;
    ButtonClose: TButton;
    procedure ButtonTableClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    FMainApp: IMainApp;
    FTableVirtualTradeFrame: TTableVirtualTradeFrame;
    procedure InitFrame;
  protected
    property TableVirtualTradeFrame: TTableVirtualTradeFrame read FTableVirtualTradeFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MainApp: IMainApp write FMainApp;
  end;

implementation

{$R *.fmx}

procedure TTableFrame.ButtonTableClick(Sender: TObject);
begin
  TabControl.ActiveTab := TabItemVirtualTrade;
end;

constructor TTableFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitFrame;
end;

destructor TTableFrame.Destroy;
begin

  inherited;
end;

procedure TTableFrame.InitFrame;
begin
  TabControl.ActiveTab := TabItemVirtualTrade;
  FTableVirtualTradeFrame := TTableVirtualTradeFrame.Create(nil);
  FTableVirtualTradeFrame.Parent := TabItemVirtualTrade;
  FTableVirtualTradeFrame.Align := TAlignLayout.Client;
end;

procedure TTableFrame.ButtonApplyClick(Sender: TObject);
begin
  //
end;

procedure TTableFrame.ButtonCloseClick(Sender: TObject);
begin
  FMainApp.EventCloseTabControl;
end;

end.
