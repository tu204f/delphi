unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  BTMemoryModule,
  QuikTrans2Order,
  QuikTrans2QuikAPI,
  QuikTransOrder;

type
  TMainForm = class(TForm)
    ledQuickTableName: TLabeledEdit;
    ledClassCode: TLabeledEdit;
    ledSecCode: TLabeledEdit;
    ledTrdaccID: TLabeledEdit;
    LabeledEdit1: TLabeledEdit;
    LabeledEditPrice: TLabeledEdit;
    LabeledEditQty: TLabeledEdit;
    ButtonTrade: TButton;
    RadioButtonBuy: TRadioButton;
    RadioButtonSell: TRadioButton;
    Memo: TMemo;
    procedure ButtonTradeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.Setting, Lb.Logger;

procedure TMainForm.ButtonTradeClick(Sender: TObject);



var
  xMsg: String;
  xPath: String;
  xSyncOrder: TCustomSyncOrder;
  xPrice: Double;
  xQty: Integer;
  xBuySell: String;
begin
  xSyncOrder := TCustomSyncOrder.Create;
  try

    xPath := TSetting.ReadString('config.sys.path_quik','');
    GetConnectQUIK(xPath);

    // Операция
    xSyncOrder.SecCode   := TSetting.ReadString('config.sys.sec_code','');
    xSyncOrder.ClassCode := TSetting.ReadString('config.sys.class_code','');
    xSyncOrder.TrdaccID  := TSetting.ReadString('config.sys.trdacc_id','');


    xPrice := StrToFloatDef(LabeledEditPrice.Text,0);
    xQty   := StrToIntDef(LabeledEditQty.Text,0);

    if RadioButtonBuy.Checked then
      xBuySell := 'B';

    if RadioButtonSell.Checked then
      xBuySell := 'S';


    if (xPrice > 0) and (xQty > 0) then
    begin
      xSyncOrder.GetNewOrder(
        xPrice,
        xQty,
        xBuySell,
        xMsg
      );
    end;

    Memo.Lines.Add('  Price := ' + xPrice.ToString);
    Memo.Lines.Add('  Qty := ' + xQty.ToString);
    Memo.Lines.Add('  BuySell := ' + xBuySell);
    Memo.Lines.Add('');

    Memo.Lines.Add(xMsg);
  finally
    FreeAndNil(xSyncOrder);
  end;

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ledQuickTableName.Text := TSetting.ReadString('config.sys.quik_table_rsi','s_rsi');
  ledClassCode.Text      := TSetting.ReadString('config.sys.class_code','');
  ledSecCode.Text        := TSetting.ReadString('config.sys.sec_code','');
  ledTrdaccID.Text       := TSetting.ReadString('config.sys.trdacc_id','');
  LabeledEdit1.Text      := TSetting.ReadString('config.sys.path_quik','');

  LabeledEditPrice.SetFocus;
  RadioButtonBuy.Checked := True;
end;

end.
