unit UnitMainClientFrame;

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
  UnitStatusFrame,
  UnitOrderCategoryFrame,
  FMX.Objects;

type
  TMainClientFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    LayoutSell: TLayout;
    LayoutInfo: TLayout;
    LayoutBuy: TLayout;
  private
    procedure InitOrderCategoryFrame;
  protected
    OrderCategoryBuy: TOrderCategoryFrame;
    OrderCategorySell: TOrderCategoryFrame;
  public
    StatusFrame: TStatusFrame;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadParam;
    procedure SaveParam;
  end;

implementation

{$R *.fmx}

{ TMainClientFrame }

constructor TMainClientFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitOrderCategoryFrame;
end;

destructor TMainClientFrame.Destroy;
begin
  FreeAndNil(OrderCategoryBuy);
  FreeAndNil(OrderCategorySell);
  inherited;
end;

procedure TMainClientFrame.InitOrderCategoryFrame;

  function _InitOrderCategoryFrame(const ALayout: TLayout): TOrderCategoryFrame;
  var
    xFrame: TOrderCategoryFrame;
  begin
    xFrame := TOrderCategoryFrame.Create(nil);
    xFrame.Parent := ALayout;
    xFrame.Align := TAlignLayout.Client;
    Result := xFrame;
  end;

  function _InitStatusFrame: TStatusFrame;
  var
    xFrame: TStatusFrame;
  begin
    xFrame := TStatusFrame.Create(nil);
    xFrame.Parent := LayoutInfo;
    xFrame.Align := TAlignLayout.Client;
    Result := xFrame;
  end;


begin
  OrderCategoryBuy  := _InitOrderCategoryFrame(LayoutBuy);
  OrderCategorySell := _InitOrderCategoryFrame(LayoutSell);
  StatusFrame := _InitStatusFrame;
end;

procedure TMainClientFrame.LoadParam;
begin
  // Загузить параметры
end;

procedure TMainClientFrame.SaveParam;
begin
  // Сохранить параметры
end;

end.
