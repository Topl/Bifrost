var Loan = function(principal, accruedInterest, paymentInterval, interestRate, interestInterval, startingDate,
                    totalPaid, lastPayment, nextPayment) {
    this.principal = principal;
    this.accruedInterest = accruedInterest;
    this.paymentInterval = paymentInterval;
    this.interestRate = interestRate;
    this.interestInterval = interestInterval;
    this.startingDate = startingDate;
    this.totalPaid = totalPaid;
    this.lastPayment = lastPayment;
    this.nextPayment = nextPayment;
};

Loan.prototype.inBreach = function() {
  return (typeof this.nextPayment !== 'undefined') && (this.nextPayment);
};

Loan.prototype.isFinished = function () {
  return typeof this.nextPayment === 'undefined';
};

Loan.prototype.updated = function (newPrincipal, newAccruedInterest, newTotalPaid, newLastPayment, newNextPayment) {
  return new Loan(newPrincipal, newAccruedInterest, this.paymentInterval, this.interestRate, this.interestInterval,
    this.startingDate, newTotalPaid, newLastPayment, newNextPayment);
};

Loan.prototype.makePayment = function(amount) {
  if(this.nextPayment === 'undefined') {

  }
};

modules.export = Loan;