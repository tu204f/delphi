{*******************************************************}
{        Библиотека компонентов нейронных сетей         }
{          Neural Networks Component Library            }
{                                                       }
{           Лаборатория BaseGroup (c) 2000              }
{           Copyright (c) 2000 BaseGroup Lab            }
{*******************************************************}

unit NeuralBaseTypes;

interface

const

  DefaultAlpha = 1;              // Параметр крутизны активационной функции
  DefaultEpochCount = 10000;     // Количество эпох для обучения
  DefaultErrorValue = 0.05;      // Ошибка по умолчанию для всех видов
  DefaultHopfLayerCount = 2;     // Количество слоев в сети Хопфилда
  DefaultLayerCount = 0;         // Минимальное количество слоев в сети back-propagation
  DefaultMaxIterCount = 10;      // Максимальное количество итераций в алгоритме Хопфилда
  DefaultMomentum = 0.9;         // Импульс - момент
  DefaultNeuronCount = 0;        // Количество нейронов в скрытом слое по умолчанию
  DefaultPatternCount = 0;       // Количество примеров
  DefaultTeachRate = 0.1;        // Скорость обучения
  DefaultTeachIdentCount = 100;  // Требуемый процент распознанных примеров из обучающего множества
  DefaultTestIdentCount = 100;   // Требуемый процент распознанных примеров из тестового множества
  DefaultUseForTeach = 100;      // Процент примеров используемых в качестве обучающего множества
  DefaultDeltaBarAcceleratingConst = 0.095;
  DefaultDeltaBarDecFactor = 0.85;
  DefaultRPropInitValue = 0.1;
  DefaultRPropMaxStepSize = 50;
  DefaultRPropMinStepSize = 1E-6;
  DefaultRPropDecFactor = 0.5;
  DefaultRPropIncFactor = 1.2;
  DefaultSuperSABDecFactor = 0.5;
  DefaultSuperSABIncFactor = 1.05;

  SensorLayer = 0;               // Сенсорный слой
  BiasNeuron = 1;                // Смещение в сети back-propagation

  Separators = ['.', ','];
  Letters    = ['a'..'z'];
  Capitals   = ['A'..'Z'];
  DigitChars = ['0'..'9'];
  SpaceChar = #9;

resourcestring

  SFieldNorm = 'Не существует типа нормализации %d';
  SFieldKind = 'Не существует типа поля %d';
  SFieldIndexRange = 'Неправильно указан номер поля %d';
  SInFieldCount = 'Неправильно установлено количество входных полей';
  SInNeuronCount = 'Неправильно установлено количество входных нейронов';
  SInVectorCount = 'Неправильно установлена размерность входного вектора';
  SLayerRangeIndex = 'Неправильно указан номер слоя %d';
  SNeuronRangeIndex = 'Неправильно указан номер нейрона %d';
  SNeuronCount = 'Неправильно указано количество нейронов';
  SOutFieldCount = 'Неправильно установлено количество выходных полей';
  SOutNeuronCount = 'Неправильно установлено количество выходных нейронов';
  SOutVectorCount = 'Неправильно установлена размерность выходного вектора';
  SPatternRangeIndex = 'Выход за границы массива примеров %d';
  SStreamCannotRead = 'Ошибка чтения из потока';
  SWeightRangeIndex = 'Неправильно указан номер веса %d';
  SWrongFileName = 'Неправильно указано имя файла %s';
  SCannotBeNumber = 'Ошибка, выражение %s невозможно привести к числовому типу';
  SBPStopCondition = 'Не задано условие останова процесса обучения';

type

  TVectorInt = array of integer;
  TVectorFloat = array of double;
  TVectorString = array of string;
  TMatrixInt = array of array of integer;
  TMatrixFloat = array of array of double;
  TNormalize = (nrmLinear, nrmSigmoid, nrmAuto,  nrmNone,
                nrmLinearOut, nrmAutoOut);
  TNeuroFieldType = (fdInput, fdOutput, fdNone);

implementation


end.

