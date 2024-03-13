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
    procedure FormShow(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
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
  ledClassCode.Text := TSetting.ReadString('config.sys.class_code','');
  ledSecCode.Text := TSetting.ReadString('config.sys.sec_code','');
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

  Self.ModalResult := mrOk;
end;

end.
