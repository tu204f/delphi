unit UnitSettingQuikFrame;

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
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Edit;

type
  TSettingQuikFrame = class(TFrame)
    EditNameTable: TEdit;
    EditClass: TEdit;
    EditCode: TEdit;
    Text2: TText;
    Text3: TText;
    Text1: TText;
    Text4: TText;
    EditTrdaccID: TEdit;
    EditQuikPath: TEdit;
    Text5: TText;
    ButtonPaht: TButton;
    CheckBoxLogTrade: TCheckBox;
    OpenDialog: TOpenDialog;
    procedure ButtonPahtClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  end;

implementation

{$R *.fmx}

uses
  Lb.SysUtils;

{ TFrame1 }

procedure TSettingQuikFrame.ButtonPahtClick(Sender: TObject);
var
  xPath: String;
begin
  if OpenDialog.Execute then
  begin
    xPath := OpenDialog.FileName;
    EditQuikPath.Text := ExtractFilePath(xPath);
  end;
end;

constructor TSettingQuikFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSettingQuikFrame.Destroy;
begin

  inherited;
end;

procedure TSettingQuikFrame.Load;
begin
  EditNameTable.Text := ParamApplication.QuikTableRSI;
  EditClass.Text := ParamApplication.ClassCode;
  EditCode.Text := ParamApplication.SecCode;
  EditTrdaccID.Text := ParamApplication.TrdaccID;
  EditQuikPath.Text := ParamApplication.PathQuik;
  CheckBoxLogTrade.IsChecked := ParamApplication.IsLogTrade;
end;

procedure TSettingQuikFrame.Save;
begin
  ParamApplication.QuikTableRSI := EditNameTable.Text;
  ParamApplication.ClassCode := EditClass.Text;
  ParamApplication.SecCode := EditCode.Text;
  ParamApplication.TrdaccID := EditTrdaccID.Text;
  ParamApplication.PathQuik := EditQuikPath.Text;
  ParamApplication.IsLogTrade := CheckBoxLogTrade.IsChecked;
end;

end.
