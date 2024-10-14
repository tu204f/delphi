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
  UnitTableVirtualTrade,
  UnitQuikExportFrame,
  UnitBybitExportFrame;

type
  TTableFrame = class(TFrame)
    TabControl: TTabControl;
    TabItemVirtualTrade: TTabItem;
    LayoutBottom: TLayout;
    ButtonApply: TButton;
    ButtonClose: TButton;
    TabItemQuikTable: TTabItem;
    TabItemBybitTable: TTabItem;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    FMainApp: IMainApp;
    FTableVirtualTradeFrame: TTableVirtualTradeFrame;
    FQuikExportFrame: TQuikExportFrame;
    FBybitExportFrame: TBybitExportFrame;
    procedure InitFrame;
  protected
    property TableVirtualTradeFrame: TTableVirtualTradeFrame read FTableVirtualTradeFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property MainApp: IMainApp write FMainApp;
  public
    property BybitExportFrame: TBybitExportFrame read FBybitExportFrame;
  end;

implementation

{$R *.fmx}

uses
  Lb.Status.Bybit,
  Lb.Status.Quik;

constructor TTableFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitFrame;
end;

destructor TTableFrame.Destroy;
begin
  FreeAndNil(FTableVirtualTradeFrame);
  FreeAndNil(FQuikExportFrame);
  FreeAndNil(FBybitExportFrame);
  inherited;
end;

procedure TTableFrame.InitFrame;
begin
  TabControl.ActiveTab := TabItemVirtualTrade;
  FTableVirtualTradeFrame := TTableVirtualTradeFrame.Create(nil);
  FTableVirtualTradeFrame.Parent := TabItemVirtualTrade;
  FTableVirtualTradeFrame.Align := TAlignLayout.Client;

  FQuikExportFrame := TQuikExportFrame.Create(nil);
  FQuikExportFrame.Parent := TabItemQuikTable;
  FQuikExportFrame.Align := TAlignLayout.Client;

  FBybitExportFrame := TBybitExportFrame.Create(nil);
  FBybitExportFrame.Parent := TabItemBybitTable;
  FBybitExportFrame.Align := TAlignLayout.Client;
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
