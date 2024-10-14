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
  FMX.Edit,
  FMX.Layouts,
  UnitSettingTacticsFrame,
  UnitSettingLimitTimeFrame;

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
    OpenDialog: TOpenDialog;
    LayoutTactics: TLayout;
    LayoutTimeLimit: TLayout;
    procedure ButtonPahtClick(Sender: TObject);
  private
    SettingTactics: TSettingTacticsFrame;
    SettingLimitTime: TSettingLimitTimeFrame;
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

{ TSettingQuikFrame }

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
  // ---------------------------------
  SettingTactics := TSettingTacticsFrame.Create(nil);
  SettingTactics.Parent := LayoutTactics;
  SettingTactics.Align := TAlignLayout.Client;
  // ---------------------------------
  SettingLimitTime := TSettingLimitTimeFrame.Create(nil);
  SettingLimitTime.Parent := LayoutTimeLimit;
  SettingLimitTime.Align := TAlignLayout.Client;
end;

destructor TSettingQuikFrame.Destroy;
begin
  FreeAndNil(SettingLimitTime);
  FreeAndNil(SettingTactics);
  inherited;
end;

procedure TSettingQuikFrame.Load;
begin
  EditNameTable.Text := ParamPlatform.QuikTableRSI;
  EditClass.Text := ParamPlatform.ClassCode;
  EditCode.Text := ParamPlatform.SecCode;
  EditTrdaccID.Text := ParamPlatform.TrdaccID;
  EditQuikPath.Text := ParamPlatform.PathQuik;
  SettingTactics.Load;
  SettingLimitTime.Load;
end;

procedure TSettingQuikFrame.Save;
begin
  ParamPlatform.QuikTableRSI := EditNameTable.Text;
  ParamPlatform.ClassCode := EditClass.Text;
  ParamPlatform.SecCode := EditCode.Text;
  ParamPlatform.TrdaccID := EditTrdaccID.Text;
  ParamPlatform.PathQuik := EditQuikPath.Text;
  SettingTactics.Save;
  SettingLimitTime.Save;
end;

end.
