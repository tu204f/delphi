unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo,
  Lb.SysUtils,
  Lb.Journal.Trading.V2, System.Rtti, FMX.Grid.Style, FMX.Grid;

type
  TMainForm = class(TForm)
    ButtonAddTradeBuy: TButton;
    ButtonAddTradeSell: TButton;
    EditQty: TEdit;
    EditPrice: TEdit;
    StrGrid: TStringGrid;
    Timer: TTimer;
    ButtonCloseTrade: TButton;
    procedure ButtonAddTradeBuyClick(Sender: TObject);
    procedure ButtonAddTradeSellClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ButtonCloseTradeClick(Sender: TObject);
  private
    procedure SetShowPosition;
    procedure SetParamTrade(var APrice, AQty: Double);
  public
    PriceValue: Double;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

var
  JournalManager: TJournalManager = nil;

procedure TMainForm.FormCreate(Sender: TObject);

  procedure AddCol(AStrGrid: TStringGrid; AHeader: String);
  var
    xCol: TStringColumn;
  begin
    xCol := TStringColumn.Create(nil);
    xCol.Parent := AStrGrid;
    xCol.Header := AHeader;
  end;

begin
  JournalManager := TJournalManager.Create;

  StrGrid.ClearColumns;
  AddCol(StrGrid,'ID');
  AddCol(StrGrid,'IsActive');
  AddCol(StrGrid,'Price');
  AddCol(StrGrid,'Qty');
  AddCol(StrGrid,'Side');
  AddCol(StrGrid,'CountTrade');
  AddCol(StrGrid,'Profit');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(JournalManager);
end;


procedure TMainForm.SetParamTrade(var APrice, AQty: Double);
begin
  APrice := StrToFloatDef(EditPrice.Text,0);
  AQty := StrToFloatDef(EditQty.Text,0);
end;

procedure TMainForm.ButtonAddTradeBuyClick(Sender: TObject);
var
  xPrice, xQty: Double;
begin
  SetParamTrade(xPrice, xQty);
  JournalManager.OpenTrade(
    Time,
    xPrice - Random(10),
    xQty,
    TTypeBuySell.tsBuy,
    nil
  );
  SetShowPosition;
end;

procedure TMainForm.ButtonAddTradeSellClick(Sender: TObject);
var
  xPrice, xQty: Double;
begin
  SetParamTrade(xPrice, xQty);
  JournalManager.OpenTrade(
    Time,
    xPrice + Random(10),
    xQty,
    TTypeBuySell.tsSell,
    nil
  );
  SetShowPosition;
end;

procedure TMainForm.ButtonCloseTradeClick(Sender: TObject);
var
  xPrice, xQty: Double;
begin
  SetParamTrade(xPrice, xQty);
  JournalManager.CloseTrade(
    Time,
    xPrice + Random(10),
    nil
  );
  SetShowPosition;
end;

procedure TMainForm.SetShowPosition;
var
  xJournalPosition: TJournalPosition;
  i, iCount: Integer;
begin
  StrGrid.RowCount := 0;
  iCount := JournalManager.Positions.Count;
  if iCount > 0 then
  begin
    StrGrid.RowCount := iCount;
    for i := 0 to iCount - 1 do
    begin
      xJournalPosition := JournalManager.Positions[i];

      StrGrid.Cells[0,i] := (i + 1).ToString;

      if xJournalPosition.IsActive then
        StrGrid.Cells[1,i] := 'Да'
      else
        StrGrid.Cells[1,i] := 'Нет';

      StrGrid.Cells[2,i] := xJournalPosition.Price.ToString;
      StrGrid.Cells[3,i] := xJournalPosition.Qty.ToString;
      StrGrid.Cells[4,i] := GetStrToSide(xJournalPosition.Side);
      StrGrid.Cells[5,i] := xJournalPosition.Trades.Count.ToString;

      if xJournalPosition.IsActive then
        StrGrid.Cells[6,i] := xJournalPosition.GetProfit(PriceValue).ToString
      else
        StrGrid.Cells[6,i] := xJournalPosition.GetProfit.ToString

    end;
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  PriceValue := 100 + Random(20);
  EditPrice.Text := PriceValue.ToString;
  SetShowPosition;
end;

end.
