unit UnitAddOrderForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Lb.SysUtils;

type
  TAddOrderForm = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ledSecCode: TLabeledEdit;
    ledCodeClass: TLabeledEdit;
    ledQuantity: TLabeledEdit;
    UpDownQuantity: TUpDown;
    ledValueRSI: TLabeledEdit;
    UpDownValueRSI: TUpDown;
    RadioGroupDirection: TRadioGroup;
    RadioGroupSide: TRadioGroup;
    ledStepPrice: TLabeledEdit;
    UpDownStepPrice: TUpDown;
    procedure FormShow(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
  private
    FUserOrder: TUserOrder;
    procedure SetShowOrder;
  public
    property UserOrder: TUserOrder read FUserOrder;
  end;

var
  AddOrderForm: TAddOrderForm;

implementation

{$R *.dfm}

uses
  Lb.Setting;

procedure TAddOrderForm.FormShow(Sender: TObject);
var
  xIndex: Integer;
begin
  FUserOrder.SecCode   := TSetting.ReadString('config.sys.sec_code','');
  FUserOrder.CodeClass := TSetting.ReadString('config.sys.class_code','');
  FUserOrder.Quantity  := TSetting.ReadInteger('config.sec.quantity',0);
  FUserOrder.ValueRSI  := TSetting.ReadInteger('config.sec.rsi',50);
  FUserOrder.StepPrice := TSetting.ReadInteger('config.sec.step_price',0);

  xIndex := TSetting.ReadInteger('config.sec.sec.sid',0);
  if (xIndex < 0) or (xIndex > 1) then
    xIndex := 0;
  RadioGroupSide.Buttons[xIndex].Checked := True;
  FUserOrder.BuySell := TTypeBuySell(xIndex);


  xIndex := TSetting.ReadInteger('config.sec.direction',0);
  if (xIndex < 0) or (xIndex > 1) then
    xIndex := 0;
  RadioGroupDirection.Buttons[xIndex].Checked := True;
  FUserOrder.Direction := TTypeDirection(xIndex);
  FUserOrder.MktLmt := toLimit;

  SetShowOrder;
end;

procedure TAddOrderForm.SetShowOrder;
begin
  ledSecCode.Text := FUserOrder.SecCode;
  ledCodeClass.Text := FUserOrder.CodeClass;
  UpDownQuantity.Position := FUserOrder.Quantity;
  UpDownValueRSI.Position := FUserOrder.ValueRSI;
  UpDownStepPrice.Position := FUserOrder.StepPrice;

  RadioGroupSide.Buttons[Integer(FUserOrder.BuySell)].Checked := True;
  RadioGroupDirection.Buttons[Integer(FUserOrder.Direction)].Checked := True;
end;

function GetIndexBottun(ARadioGroup: TRadioGroup): Integer;
begin
  Result := 0;
  if ARadioGroup.Buttons[0].Checked then
    Result := 0
  else if ARadioGroup.Buttons[1].Checked then
    Result := 1;
end;

procedure TAddOrderForm.ButtonOKClick(Sender: TObject);
var
  xIndex: Integer;
begin

  FUserOrder.Quantity := UpDownQuantity.Position;
  FUserOrder.ValueRSI := UpDownValueRSI.Position;
  FUserOrder.StepPrice := UpDownStepPrice.Position;

  xIndex := GetIndexBottun(RadioGroupSide);
  FUserOrder.BuySell   := TTypeBuySell(xIndex);

  xIndex := GetIndexBottun(RadioGroupDirection);
  FUserOrder.Direction := TTypeDirection(xIndex);

  // Применить, добавить заявку

  TSetting.WriteInteger('config.sec.quantity',FUserOrder.Quantity);
  TSetting.WriteInteger('config.sec.rsi',FUserOrder.ValueRSI);
  TSetting.WriteInteger('config.sec.step_price',FUserOrder.StepPrice);
  TSetting.WriteInteger('config.sec.sec.sid',Integer(FUserOrder.BuySell));
  TSetting.WriteInteger('config.sec.direction',Integer(FUserOrder.Direction));

  Self.ModalResult := mrOk;
end;

procedure TAddOrderForm.ButtonCancelClick(Sender: TObject);
begin
  // Откланить условия заявки
  Self.ModalResult := mrCancel;
end;

end.
