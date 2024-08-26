unit UnitToolsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.WinXPickers;

type
  TToolsForm = class(TForm)
    ledQuickTableName: TLabeledEdit;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ledClassCode: TLabeledEdit;
    ledSecCode: TLabeledEdit;
    ledTrdaccID: TLabeledEdit;
    LabeledEdit1: TLabeledEdit;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    CheckBoxLogTrade: TCheckBox;
    TimePickerBegin: TTimePicker;
    TimePickerEnd: TTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    LabeledEditTakeProfit: TLabeledEdit;
    LabeledEditStopLoss: TLabeledEdit;
    procedure FormShow(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ToolsForm: TToolsForm;

implementation

{$R *.dfm}

uses
  Lb.Setting;

procedure TToolsForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Настройка работы проекта';
  // ATrdaccID
  // AClassCode
  // ASecCode
  ledQuickTableName.Text   := TSetting.ReadString('config.sys.quik_table_rsi','s_rsi');
  ledClassCode.Text        := TSetting.ReadString('config.sys.class_code','');
  ledSecCode.Text          := TSetting.ReadString('config.sys.sec_code','');
  ledTrdaccID.Text         := TSetting.ReadString('config.sys.trdacc_id','');
  LabeledEdit1.Text        := TSetting.ReadString('config.sys.path_quik','');
  CheckBoxLogTrade.Checked := TSetting.ReadBool('config.sys.is_log_trade',True);
  // Время когда нужно остановить торговые операции
  TimePickerBegin.Time     := TSetting.ReadTime('config.sys.time_begin',StrToTime('10:00:00'));
  TimePickerEnd.Time       := TSetting.ReadTime('config.sys.time_end',StrToTime('18:00:00'));
  // Глобальное получения прибыли или убытка
  // Останавливаем совершение торговых оперций
  LabeledEditTakeProfit.Text := FloatToStr(TSetting.ReadFloat('config.sys.global_tk',100));
  LabeledEditStopLoss.Text   := FloatToStr(TSetting.ReadFloat('config.sys.global_sl',-100));
end;

procedure TToolsForm.Button1Click(Sender: TObject);
var
  xFileName: String;
begin
  if OpenDialog1.Execute then
  begin
    xFileName := OpenDialog1.FileName;
    LabeledEdit1.Text := xFileName;
  end;
end;

procedure TToolsForm.ButtonCancelClick(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TToolsForm.ButtonOKClick(Sender: TObject);
begin
  TSetting.WriteString('config.sys.quik_table_rsi',ledQuickTableName.Text);

  TSetting.WriteString('config.sys.class_code',ledClassCode.Text);
  TSetting.WriteString('config.sys.sec_code',ledSecCode.Text);
  TSetting.WriteString('config.sys.trdacc_id',ledTrdaccID.Text);
  TSetting.WriteString('config.sys.path_quik',LabeledEdit1.Text);
  TSetting.WriteBool('config.sys.is_log_trade',CheckBoxLogTrade.Checked);

  // Время когда нужно остановить торговые операции
  TSetting.WriteTime('config.sys.time_begin',TimePickerBegin.Time);
  TSetting.WriteTime('config.sys.time_end',TimePickerEnd.Time);
  // Глобальное получения прибыли или убытка
  // Останавливаем совершение торговых оперций
  TSetting.WriteFloat('config.sys.global_tk',StrToFloatDef(LabeledEditTakeProfit.Text,500));
  TSetting.WriteFloat('config.sys.global_sl',StrToFloatDef(LabeledEditStopLoss.Text,-100));

  Self.ModalResult := mrOk;
end;

end.
