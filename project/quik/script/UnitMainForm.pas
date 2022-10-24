unit UnitMainForm;

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
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TScriptForm = class(TForm)
    ButtonCreateScript: TButton;
    ledTitli: TLabeledEdit;
    ledDescription: TLabeledEdit;
    ledClassCode: TLabeledEdit;
    ledSecCode: TLabeledEdit;
    cbInterval: TComboBox;
    ledCountBar: TLabeledEdit;
    UpDown: TUpDown;
    SaveDialog: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure ButtonCreateScriptClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ScriptForm: TScriptForm;

implementation

{$R *.dfm}

uses
  Lb.Script.QPile,
  Lb.Setting;

procedure TScriptForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Программа генерацию — Скрипта';

  ledTitli.Text        := TSetting.ReadString('config.sys.title','');
  ledDescription.Text  := TSetting.ReadString('config.sys.description','');
  ledSecCode.Text      := TSetting.ReadString('config.sys.seccode','');
  ledClassCode.Text    := TSetting.ReadString('config.sys.classcode','');
  cbInterval.ItemIndex := TSetting.ReadInteger('config.sys.interval',0);
  UpDown.Position      := TSetting.ReadInteger('config.sys.countbar',20);
  SaveDialog.InitialDir  := TSetting.ReadString('config.sys.qpl_dir','');

end;

procedure TScriptForm.ButtonCreateScriptClick(Sender: TObject);

  procedure _SaveParam;
  begin
    // Так делать не нужно
    TSetting.WriteString('config.sys.title',ledTitli.Text);
    TSetting.WriteString('config.sys.description',ledDescription.Text);
    TSetting.WriteString('config.sys.seccode',ledSecCode.Text);
    TSetting.WriteString('config.sys.classcode',ledClassCode.Text);
    TSetting.WriteInteger('config.sys.interval',cbInterval.ItemIndex);
    TSetting.WriteInteger('config.sys.countbar',UpDown.Position);
    TSetting.WriteString('config.sys.qpl_dir',ExtractFilePath(SaveDialog.FileName));
  end;

  function _Interval: Integer;
  begin
    case cbInterval.ItemIndex of
        1: Result := 2;
        2: Result := 3;
        3: Result := 4;
        4: Result := 5;
        5: Result := 6;
        6: Result := 10;
        7: Result := 15;
        8: Result := 20;
        9: Result := 30;
       10: Result := 60;
       11: Result := 120;
       12: Result := 240;
    else
      Result := 1;
    end;
  end;

var
  xStr: TStrings;
  xInterval: Integer;
begin
  if SaveDialog.Execute then
  begin
    xStr := TStringList.Create;
    try
      xInterval := _Interval;
      xStr.Text := GetResourceScritpQPL(
        ledTitli.Text,
        ledDescription.Text,
        ledSecCode.Text,
        ledClassCode.Text,
        xInterval,
        UpDown.Position
      );
      xStr.SaveToFile(SaveDialog.FileName);
      _SaveParam;
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

end.
