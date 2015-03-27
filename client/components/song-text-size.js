import React from 'react';

export default React.createClass({

    render: function () {
        return (
            <li>
                <button className="fa fa-toggle-down fa-2x btn btn-default" onClick={this.props.onDecrement} value="-" />
                <button className="fa fa-toggle-up fa-2x btn btn-default" onClick={this.props.onIncrement} value="+" />
            </li>
        );
    }

});
