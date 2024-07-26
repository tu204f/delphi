unit UnitToolsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

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
  ledQuickTableName.Text := TSetting.ReadString('config.sys.quik_table_rsi','s_rsi');
  ledClassCode.Text      := TSetting.ReadString('config.sys.class_code','');
  ledSecCode.Text        := TSetting.ReadString('config.sys.sec_code','');
  ledTrdaccID.Text       := TSetting.ReadString('config.sys.trdacc_id','');
  LabeledEdit1.Text      := TSetting.ReadString('config.sys.path_quik','');
  CheckBoxLogTrade.Checked := TSetting.ReadBool('config.sys.is_log_trade',True);
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

  Self.ModalResult := mrOk;
end;

end.
