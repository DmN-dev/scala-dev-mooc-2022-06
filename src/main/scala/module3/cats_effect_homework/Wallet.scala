package module3.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import module3.cats_effect_homework.Wallet._

import java.nio.file.{Files, Path, Paths}

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {

  val path: Path = Paths.get(s"src/main/resources/$id.txt")
  def createFile(initAmount: BigDecimal = 0.0): Unit = {
    def initializeWallet(amount: BigDecimal): Unit = Files.writeString(path, amount.toString)
    if (!Files.exists(path)) { Files.createFile(path); initializeWallet(initAmount) }
  }
  def balance: F[BigDecimal] = Sync[F].delay(BigDecimal(Files.readString(path)))
  def topup(amount: BigDecimal): F[Unit] = {
    for {
      balanceRes <- balance
      _ <- Sync[F].delay(Files.writeString(path, (balanceRes + amount).toString))
    } yield ()
  }

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = {
    for {
      balanceRes <- balance
      delta <- Sync[F].delay(balanceRes - amount)
      res <- if (delta >= 0 ) {
        Sync[F].delay(Files.writeString(path, delta.toString)).as(Right())
      } else {
        Sync[F].pure(Left(BalanceTooLow))
      }
    } yield res
  }
}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] =
    for {
      fileWallet <- Sync[F].delay(new FileWallet(id))
      _ <- Sync[F].delay(fileWallet.createFile())
    } yield fileWallet

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
