unit UnitSettingBybitFrame;

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
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Objects,
  FMX.Layouts,
  UnitSettingTacticsFrame,
  FMX.ListBox,
  UnitSettingLimitTimeFrame;

type
  ///<summary>
  /// ��������� ��� ������
  ///</summary>
  TSettingBybitFrame = class(TFrame)
    Text1: TText;
    Text3: TText;
    Text2: TText;
    EditSymble: TEdit;
    EditApiSecret: TEdit;
    EditApiKey: TEdit;
    LayoutTactics: TLayout;
    ComboBoxInterval: TComboBox;
    Text4: TText;
    LayoutTimeLimit: TLayout;
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
  Lb.Bybit.SysUtils,
  Lb.SysUtils;

{ TSettingBybitFrame }

constructor TSettingBybitFrame.Create(AOwner: TComponent);
begin
  inherited;
  SettingTactics := TSettingTacticsFrame.Create(nil);
  SettingTactics.Parent := LayoutTactics;
  SettingTactics.Align := TAlignLayout.Client;

  SettingLimitTime := TSettingLimitTimeFrame.Create(nil);
  SettingLimitTime.Parent := LayoutTimeLimit;
  SettingLimitTime.Align := TAlignLayout.Client;
end;

destructor TSettingBybitFrame.Destroy;
begin
  FreeAndNil(SettingLimitTime);
  FreeAndNil(SettingTactics);
  inherited;
end;

procedure TSettingBybitFrame.Load;

  procedure _InitInterval;
  begin
    with ComboBoxInterval.Items do
    begin
      Clear;
      Add('������');
      Add('3 ������');
      Add('5 �����');
      Add('15 �����');
      Add('30 �����');
      Add('���');
      Add('2 ����');
      Add('4 ����');
      Add('6 ����');
      Add('12 ����');
      Add('����');
      Add('������');
      Add('�����');
    end;
  end;

begin
  _InitInterval;
  EditSymble.Text    := ParamPlatform.Symble;
  EditApiKey.Text    := ParamPlatform.ApiKey;
  EditApiSecret.Text := ParamPlatform.ApiSecret;
  ComboBoxInterval.ItemIndex := Integer(ParamPlatform.Interval);
  SettingTactics.Load;
  SettingLimitTime.Load;
end;

procedure TSettingBybitFrame.Save;
begin
  ParamPlatform.Interval  := TTypeInterval(ComboBoxInterval.ItemIndex);
  ParamPlatform.Symble    := EditSymble.Text;
  ParamPlatform.ApiKey    := EditApiKey.Text;
  ParamPlatform.ApiSecret := EditApiSecret.Text;
  SettingTactics.Save;
  SettingLimitTime.Save;
end;

end.
