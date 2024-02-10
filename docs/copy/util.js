function dateToString(date, includeYear = false) {
  const monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  ];
  return `${monthNames[date.getMonth()]} ${date.getDate()}${
    includeYear ? ', ' + date.getFullYear() : ''}`;
}
