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
  FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    ButtonBuy: TButton;
    ButtonSell: TButton;
    SpinBoxPrice: TSpinBox;
    SpinBoxQty: TSpinBox;
    Text1: TText;
    Text2: TText;
    Memo1: TMemo;
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
    procedure ButtonBuyClick(Sender: TObject);
    procedure ButtonSellClick(Sender: TObject);
    procedure ButtonBuyQtyClick(Sender: TObject);
    procedure ButtonSellQtyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetLogics;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.VirtualTrade;

procedure TMainForm.ButtonBuyClick(Sender: TObject);
begin
  Virtual_SelectedOrder(
    'TEST_SYMBLE',      // Òîğãîâûé ñèìâîë
    TQBTypeSide.tsBuy,  // Íàïğîâëåíèå òîğãîâîãî îïåğöèé
    SpinBoxQty.Value,   // Êîëè÷åñòâî
    SpinBoxPrice.Value, // Öåíà
    'BUY'               // Íàïğîâëåíèå îáúåêòà
  );
  SetLogics;
end;

procedure TMainForm.ButtonBuyQtyClick(Sender: TObject);
begin
  Virtual_SelectedOrder(
    'TEST_SYMBLE',      // Òîğãîâûé ñèìâîë
    TQBTypeSide.tsBuy,  // Íàïğîâëåíèå òîğãîâîãî îïåğöèé
    StrToFloatDef(TButton(Sender).Text,0),
    SpinBoxPrice.Value, // Öåíà
    'BUY'               // Íàïğîâëåíèå îáúåêòà
  );
  SetLogics;
end;

procedure TMainForm.ButtonSellClick(Sender: TObject);
begin
  Virtual_SelectedOrder(
    'TEST_SYMBLE',       // Òîğãîâûé ñèìâîë
    TQBTypeSide.tsSell,  // Íàïğîâëåíèå òîğãîâîãî îïåğöèé
    SpinBoxQty.Value,    // Êîëè÷åñòâî
    SpinBoxPrice.Value,  // Öåíà
    'SELL'               // Íàïğîâëåíèå îáúåêòà
  );
  SetLogics;
end;

procedure TMainForm.ButtonSellQtyClick(Sender: TObject);
begin
  Virtual_SelectedOrder(
    'TEST_SYMBLE',       // Òîğãîâûé ñèìâîë
    TQBTypeSide.tsSell,  // Íàïğîâëåíèå òîğãîâîãî îïåğöèé
    StrToFloatDef(TButton(Sender).Text,0),
    SpinBoxPrice.Value,  // Öåíà
    'SELL'               // Íàïğîâëåíèå îáúåêòà
  );
  SetLogics;
end;


procedure TMainForm.SetLogics;

  procedure _Log(S: String);
  begin
    Memo1.Lines.Add(S);
  end;

var
  i, iCount: Integer;
  j, jCount: Integer;
  xVirtualTrades: TVirtualTrades;
  xPostionTrade: TPostionTrade;
  xParamTrade: TParamTrade;
begin
  xVirtualTrades := GetVirtualTrades;

  Memo1.Lines.Clear;
  _Log('Êîëè÷åñòâîå îòğûòûõ ïîçèöèé' + xVirtualTrades.Count.ToString);

  _Log('Ğàçìåğ ïîçèöèè:' + xVirtualTrades.Qty.ToString);
  _Log('Íàïğîâëåíèå ïîçöèè:' + GetStrToTypeSide(xVirtualTrades.Side));


  iCount := xVirtualTrades.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xPostionTrade := xVirtualTrades.Items[i];
      _Log('PostionTrade: ' + i.ToString);

      jCount := xPostionTrade.Items.Count;
      if jCount > 0 then
        for j := 0 to jCount - 1 do
        begin
          xParamTrade := xPostionTrade.Items[j];
          _Log('  >>trade_begin[' + j.ToString + ']');
          _Log('    * Time = ' + DateTimeToStr(xParamTrade.Time));
          _Log('    * Symbol = ' + xParamTrade.Symbol);
          _Log('    * Side = ' + GetStrToTypeSide(xParamTrade.Side));
          _Log('    * Qty = ' + xParamTrade.Qty.ToString);
          _Log('    * Price = ' + xParamTrade.Price.ToString);
          _Log('    * OrderLinkId = ' + xParamTrade.OrderLinkId);
          _Log('    * TypeTrade = ' + GetStrToTypeTrade(xParamTrade.TypeTrade));
          _Log('    * Profit = ' + xParamTrade.Profit.ToString);
          _Log('  >>end');
        end;
    end;
end;

end.
