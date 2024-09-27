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
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  System.Rtti,
  FMX.Grid.Style,
  FMX.Grid,
  FMX.Layouts,
  FMX.TreeView;

type
  TMainForm = class(TForm)
    ButtonBuy: TButton;
    ButtonSell: TButton;
    SpinBoxPrice: TSpinBox;
    SpinBoxQty: TSpinBox;
    Text1: TText;
    Text2: TText;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    TreeView: TTreeView;
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonBuyQtyClick(Sender: TObject);
    procedure ButtonSellQtyClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetLogics;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.VirtualTrade.V2;

var
  localParamPositions: TParamPositions = nil;

function Vir_SelectedOrder(
  ASymbol: String;     // Торговый символ
  ASide: TQBTypeSide;  // Напровление торгового оперций
  AQty: Double;        // Количество
  APrice: Double;      // Цена
  AOrderLinkId: String // Напровление объекта
): String;
var
  xParamTrade: TParamTrade;
begin
  if not Assigned(localParamPositions) then
  begin
    Result := 'Error.Ok';
    Exit;
  end;

  try
    xParamTrade.Date := Date;
    xParamTrade.Time := Time;
    xParamTrade.Symbol := ASymbol;
    xParamTrade.Side := ASide;
    xParamTrade.Qty := AQty;
    xParamTrade.Price := APrice;
    xParamTrade.OrderLinkId := AOrderLinkId;
    localParamPositions.AddTrade(xParamTrade);
    Result := 'OK.';
  except
    Result := 'Error.';
  end;
end;


constructor TMainForm.Create(AOwner: TComponent);

  procedure _AddCol(AGrid: TStringGrid; AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(AGrid);
    xCol.Header := AHeader;
    xCol.Parent := AGrid;
  end;

begin
  inherited;

  localParamPositions := TParamPositions.Create;

//  StrGrid.ClearColumns;
//  _AddCol(StrGrid,'Time');
//  _AddCol(StrGrid,'Symbol');
//  _AddCol(StrGrid,'Side');
//  _AddCol(StrGrid,'Qty');
//  _AddCol(StrGrid,'Price');
//  _AddCol(StrGrid,'OrderLinkId');
//  _AddCol(StrGrid,'TypeTrade');
//  _AddCol(StrGrid,'Profit');

  TreeView.ExpandAll;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(localParamPositions);
  inherited;
end;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
begin
  Vir_SelectedOrder(
    'TEST_SYMBLE',      // Торговый символ
    TQBTypeSide.tsBuy,  // Напровление торгового оперций
    SpinBoxQty.Value,   // Количество
    SpinBoxPrice.Value, // Цена
    'BUY'               // Напровление объекта
  );
  SetLogics;
end;

procedure TMainForm.ButtonBuyQtyClick(Sender: TObject);
begin
  SpinBoxPrice.Value := SpinBoxPrice.Value - 10;
  Vir_SelectedOrder(
    'TEST_SYMBLE',      // Торговый символ
    TQBTypeSide.tsBuy,  // Напровление торгового оперций
    StrToFloatDef(TButton(Sender).Text,0),
    SpinBoxPrice.Value, // Цена
    'BUY'               // Напровление объекта
  );
  SetLogics;
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
begin

  Vir_SelectedOrder(
    'TEST_SYMBLE',       // Торговый символ
    TQBTypeSide.tsSell,  // Напровление торгового оперций
    SpinBoxQty.Value,    // Количество
    SpinBoxPrice.Value,  // Цена
    'SELL'               // Напровление объекта
  );
  SetLogics;
end;

procedure TMainForm.ButtonSellQtyClick(Sender: TObject);
begin
  SpinBoxPrice.Value := SpinBoxPrice.Value + 10;
  Vir_SelectedOrder(
    'TEST_SYMBLE',       // Торговый символ
    TQBTypeSide.tsSell,  // Напровление торгового оперций
    StrToFloatDef(TButton(Sender).Text,0),
    SpinBoxPrice.Value,  // Цена
    'SELL'               // Напровление объекта
  );
  SetLogics;
end;

procedure TMainForm.SetLogics;

  procedure _ParamPosition(AItem: TTreeViewItem; AParamPosition: TParamPosition);
  var
    i, iCount: Integer;
    xParamTrade: TParamTrade;
    xItem: TTreeViewItem;
  begin
    iCount := AParamPosition.HistoryTrades.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xParamTrade := AParamPosition.HistoryTrades[i];
        xItem := TTreeViewItem.Create(nil);
        xItem.Text := xParamTrade.ToStr;
        xItem.Parent := AItem;
      end;
  end;

var
  i, iCount: Integer;
  xParamPosition: TParamPosition;
  xItem: TTreeViewItem;
begin
  TreeView.BeginUpdate;
  try
    TreeView.Clear;

    iCount := localParamPositions.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xParamPosition := localParamPositions.Positions[i];
        xItem := TTreeViewItem.Create(nil);
        xItem.Parent := TreeView;
        if xParamPosition.Qty > 0 then
        begin
          xItem.Text := 'pos: ' +
          GetStrToTypeSide(xParamPosition.Side) + ' ' +
          xParamPosition.Qty.ToString + ' ' +
          xParamPosition.Price.ToString;
        end
        else
        begin
          xItem.Text := 'pos: profit = ' +
          xParamPosition.Profit.ToString;
        end;
        _ParamPosition(xItem,xParamPosition);
      end;

  finally
    TreeView.EndUpdate;
  end;
end;

end.
