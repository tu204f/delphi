unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Media, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  UnitBeatsPerMinuteFrame,
  UnitNumberBitFrame;

type
  TMainForm = class(TForm)
    GridPanelLayout: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    MediaPlayer: TMediaPlayer;
    Timer: TTimer;
    StyleBook: TStyleBook;
    Layout3: TLayout;
    ButtonStart: TButton;
    ButtonStop: TButton;
    GridPanelTools: TGridPanelLayout;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    { Private declarations }
    FNumberBitFrame: TNumberBitFrame;
    FBeatsPerMinuteFrame: TBeatsPerMinuteFrame;
    FMediaPlayer: TMediaPlayer;
  protected
    procedure EventBeatPerMinute(Sender: TObject);
    property NumberBitFrame: TNumberBitFrame read FNumberBitFrame;
    property BeatsPerMinuteFrame: TBeatsPerMinuteFrame read FBeatsPerMinuteFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FNumberBitFrame := TNumberBitFrame.Create(nil);
  FNumberBitFrame.Parent := Layout1;
  FNumberBitFrame.Align  := TAlignLayout.Client;

  FBeatsPerMinuteFrame := TBeatsPerMinuteFrame.Create(nil);
  FBeatsPerMinuteFrame.Parent := Layout2;
  FBeatsPerMinuteFrame.Align := TAlignLayout.Client;
  FBeatsPerMinuteFrame.OnBeatPer := EventBeatPerMinute;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FMediaPlayer);
  FreeAndNil(FBeatsPerMinuteFrame);
  FreeAndNil(FNumberBitFrame);
  inherited;
end;

procedure TMainForm.EventBeatPerMinute(Sender: TObject);

  procedure _MediaPlayer;
  begin
    if Assigned(FMediaPlayer) then
      FreeAndNil(FMediaPlayer);
    FMediaPlayer := TMediaPlayer.Create(nil);
    FMediaPlayer.FileName := 'soud.mp3';

    case FNumberBitFrame.NumberBeat of
      1: FMediaPlayer.Volume := 1;
      5: FMediaPlayer.Volume := 0.8;
    else
      FMediaPlayer.Volume := 0.67;
    end;
    FMediaPlayer.Play;
  end;

begin
  _MediaPlayer;
  FNumberBitFrame.SetBeat;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  FNumberBitFrame.SetStart;
  FBeatsPerMinuteFrame.Start;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  FBeatsPerMinuteFrame.Stop;
end;

end.
